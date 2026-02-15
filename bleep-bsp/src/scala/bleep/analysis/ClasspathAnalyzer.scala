package bleep.analysis

import java.nio.file.{FileSystems, Files, Path}
import java.security.MessageDigest
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import scala.util.Using
import org.objectweb.asm.{AnnotationVisitor, ClassReader, ClassVisitor, Opcodes}
import kotlin.metadata.jvm.{JvmMetadataUtil, KotlinClassMetadata}
import kotlin.metadata.{Attributes, KmClass, KmClassifier, KmFunction, KmProperty, KmType}

/** A fully qualified name symbol for classpath analysis */
case class Symbol(fqn: String) extends Ordered[Symbol] {
  override def toString: String = fqn
  override def compare(that: Symbol): Int = this.fqn.compare(that.fqn)
}

object Symbol {
  given Ordering[Symbol] = Ordering.by(_.fqn)
}

/** Analyzes types from classpath JARs to detect library changes.
  *
  * For incremental compilation, we need to detect when external dependencies change (e.g., cats 2.9.0 → 2.10.0). This analyzer reads type definitions from
  * classpath JARs and computes hashes for change detection.
  */
object ClasspathAnalyzer {

  /** Hash of a type definition from the classpath */
  case class TypeHash(
      symbol: Symbol,
      hash: String,
      source: Path // JAR file containing this type
  )

  /** Result of analyzing a classpath */
  case class ClasspathAnalysis(
      typeHashes: Map[Symbol, TypeHash]
  ) {
    def hashFor(symbol: Symbol): Option[String] =
      typeHashes.get(symbol).map(_.hash)
  }

  /** Analyze types from classpath JARs.
    *
    * @param classpath
    *   JAR files to analyze
    * @param symbols
    *   Symbols to look up (if empty, analyzes all types)
    * @return
    *   Analysis containing type hashes
    */
  def analyze(classpath: Seq[Path], symbols: Set[Symbol]): ClasspathAnalysis = {
    val hashes = mutable.Map[Symbol, TypeHash]()

    for jar <- classpath if Files.exists(jar) && jar.toString.endsWith(".jar") do analyzeJar(jar, symbols, hashes)

    ClasspathAnalysis(hashes.toMap)
  }

  private def analyzeJar(
      jarPath: Path,
      symbols: Set[Symbol],
      hashes: mutable.Map[Symbol, TypeHash]
  ): Unit =
    Using(new JarFile(jarPath.toFile)) { jar =>
      val entries = jar.entries().asScala.toList

      // Check for TASTy files (Scala 3)
      val tastyEntries = entries.filter(_.getName.endsWith(".tasty"))
      // Check for Kotlin module files (Kotlin)
      val kotlinModuleEntries = entries.filter(_.getName.endsWith(".kotlin_module"))

      if tastyEntries.nonEmpty then analyzeTastyJar(jarPath, symbols, hashes)
      else if kotlinModuleEntries.nonEmpty then
        // Kotlin JAR - use kotlin-metadata-jvm for rich type info
        analyzeKotlinJar(jarPath, jar, symbols, hashes)
      else
        // Fall back to class file analysis (Java/Scala 2)
        analyzeClassJar(jarPath, jar, symbols, hashes)
    }.getOrElse(())

  /** Analyze a JAR containing TASTy files (Scala 3) */
  private def analyzeTastyJar(
      jarPath: Path,
      symbols: Set[Symbol],
      hashes: mutable.Map[Symbol, TypeHash]
  ): Unit =
    try {
      val ctx = createTastyContext(Seq(jarPath))
      given tastyquery.Contexts.Context = ctx

      if symbols.isEmpty then
        // Analyze all packages
        analyzeAllTastyTypes(ctx, jarPath, hashes)
      else
        // Analyze only requested symbols
        for symbol <- symbols do analyzeTastySymbol(ctx, symbol, jarPath, hashes)
    } catch {
      case e: Exception =>
        // Log but continue - some JARs may not be readable
        System.err.println(s"Warning: Could not analyze TASTy in $jarPath: ${e.getMessage}")
    }

  /** Create a tasty-query context for reading TASTy files */
  private def createTastyContext(classpath: Seq[Path]): tastyquery.Contexts.Context = {
    import tastyquery.jdk.ClasspathLoaders

    // Get JRE path for java.base module (required by tasty-query)
    val jrtPath = FileSystems
      .getFileSystem(java.net.URI.create("jrt:/"))
      .getPath("modules", "java.base")

    val allPaths = classpath :+ jrtPath
    val cp = ClasspathLoaders.read(allPaths.toList)
    tastyquery.Contexts.Context.initialize(cp)
  }

  /** Analyze all types in a TASTy context */
  private def analyzeAllTastyTypes(
      ctx: tastyquery.Contexts.Context,
      jarPath: Path,
      hashes: mutable.Map[Symbol, TypeHash]
  )(using tastyquery.Contexts.Context): Unit = {
    import tastyquery.Symbols.*

    def processPackage(pkg: PackageSymbol): Unit =
      // Process declarations in this package
      for decl <- pkg.declarations do
        decl match {
          case cls: ClassSymbol =>
            val sym = Symbol(cls.displayFullName)
            val hash = hashClassSymbol(cls)
            hashes(sym) = TypeHash(sym, hash, jarPath)
          case typeMember: TypeMemberSymbol =>
            val sym = Symbol(typeMember.displayFullName)
            val hash = hashTypeMemberSymbol(typeMember)
            hashes(sym) = TypeHash(sym, hash, jarPath)
          case pkg: PackageSymbol =>
            processPackage(pkg)
          case _ => ()
        }

    // Start from root packages
    for pkg <- ctx.defn.RootPackage.declarations.collect { case p: PackageSymbol => p } do processPackage(pkg)
  }

  /** Analyze a specific symbol from TASTy */
  private def analyzeTastySymbol(
      ctx: tastyquery.Contexts.Context,
      symbol: Symbol,
      jarPath: Path,
      hashes: mutable.Map[Symbol, TypeHash]
  )(using tastyquery.Contexts.Context): Unit = {
    import tastyquery.Symbols.*

    // Parse the FQN to find the symbol
    val parts = symbol.fqn.split('.')
    if parts.isEmpty then () // Nothing to do
    else
      try
        // Navigate to the package/class hierarchy
        navigateToSymbol(ctx.defn.RootPackage, parts.toList) match {
          case Some(cls: ClassSymbol) =>
            val hash = hashClassSymbol(cls)
            hashes(symbol) = TypeHash(symbol, hash, jarPath)
          case Some(typeMember: TypeMemberSymbol) =>
            val hash = hashTypeMemberSymbol(typeMember)
            hashes(symbol) = TypeHash(symbol, hash, jarPath)
          case _ => ()
        }
      catch {
        case _: Exception => () // Symbol not found in this JAR
      }
  }

  /** Navigate through packages/classes to find a symbol */
  private def navigateToSymbol(
      start: tastyquery.Symbols.DeclaringSymbol,
      parts: List[String]
  )(using tastyquery.Contexts.Context): Option[tastyquery.Symbols.Symbol] = {
    import tastyquery.Names.*
    import tastyquery.Symbols.*

    parts match {
      case Nil         => None
      case last :: Nil =>
        // Final part - look up as type or term
        val typeOpt = start.getDecl(typeName(last))
        val termOpt = start.getDecl(termName(last))
        typeOpt.orElse(termOpt)
      case head :: tail =>
        // Navigate through hierarchy
        val termOpt = start.getDecl(termName(head))
        val typeOpt = start.getDecl(typeName(head))
        termOpt match {
          case Some(pkg: PackageSymbol) => navigateToSymbol(pkg, tail)
          case _ =>
            typeOpt match {
              case Some(cls: ClassSymbol) => navigateToSymbol(cls, tail)
              case _                      => None
            }
        }
    }
  }

  /** Hash a class symbol */
  private def hashClassSymbol(cls: tastyquery.Symbols.ClassSymbol)(using
      tastyquery.Contexts.Context
  ): String = {
    val sb = new StringBuilder
    sb.append("class:")
    sb.append(cls.displayFullName)

    // Include parent types
    for parent <- cls.parentClasses do {
      sb.append(":parent:")
      sb.append(parent.displayFullName)
    }

    // Include type parameters
    for tparam <- cls.typeParams do {
      sb.append(":tparam:")
      sb.append(tparam.name.toString)
    }

    // Include declared members (public API)
    for decl <- cls.declarations.toSeq.sortBy(_.name.toString) do
      decl match {
        case m: tastyquery.Symbols.TermSymbol if !m.name.toString.startsWith("$") =>
          sb.append(":member:")
          sb.append(m.name.toString)
          sb.append(":")
          sb.append(m.declaredType.toString)
        case t: tastyquery.Symbols.TypeMemberSymbol =>
          sb.append(":type:")
          sb.append(t.name.toString)
          sb.append(":")
          sb.append(t.typeDef.toString)
        case _ => ()
      }

    computeHash(sb.toString)
  }

  /** Hash a type member symbol (type alias, abstract type) */
  private def hashTypeMemberSymbol(typeMember: tastyquery.Symbols.TypeMemberSymbol)(using
      tastyquery.Contexts.Context
  ): String = {
    val sb = new StringBuilder
    sb.append("type:")
    sb.append(typeMember.displayFullName)
    sb.append(":")
    sb.append(typeMember.typeDef.toString)
    computeHash(sb.toString)
  }

  /** Analyze a JAR using class files (Java/Kotlin/Scala 2) */
  private def analyzeClassJar(
      jarPath: Path,
      jar: JarFile,
      symbols: Set[Symbol],
      hashes: mutable.Map[Symbol, TypeHash]
  ): Unit = {
    val entries = jar.entries().asScala.toList
    val classEntries = entries.filter(e => e.getName.endsWith(".class") && !e.getName.contains("$"))

    for entry <- classEntries do
      try {
        val is = jar.getInputStream(entry)
        try {
          val reader = new ClassReader(is)
          val className = reader.getClassName.replace('/', '.')
          val sym = Symbol(className)

          // Skip if we have specific symbols and this isn't one of them
          if symbols.nonEmpty && !symbols.contains(sym) then () // skip
          else {
            val hash = hashClassFile(reader)
            hashes(sym) = TypeHash(sym, hash, jarPath)
          }
        } finally is.close()
      } catch {
        case _: Exception => () // Skip unreadable classes
      }
  }

  /** Hash a class file using ASM */
  private def hashClassFile(reader: org.objectweb.asm.ClassReader): String = {
    import org.objectweb.asm.{ClassVisitor, MethodVisitor, FieldVisitor, Opcodes}

    val sb = new StringBuilder
    sb.append("class:")
    sb.append(reader.getClassName)

    // Superclass
    if reader.getSuperName != null then {
      sb.append(":super:")
      sb.append(reader.getSuperName)
    }

    // Interfaces
    for iface <- reader.getInterfaces.sorted do {
      sb.append(":implements:")
      sb.append(iface)
    }

    // Visit class to extract members
    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitField(
          access: Int,
          name: String,
          descriptor: String,
          signature: String,
          value: Any
      ): FieldVisitor = {
        if (access & Opcodes.ACC_PUBLIC) != 0 || (access & Opcodes.ACC_PROTECTED) != 0 then {
          sb.append(":field:")
          sb.append(name)
          sb.append(":")
          sb.append(descriptor)
        }
        null
      }

      override def visitMethod(
          access: Int,
          name: String,
          descriptor: String,
          signature: String,
          exceptions: Array[String]
      ): MethodVisitor = {
        if (access & Opcodes.ACC_PUBLIC) != 0 || (access & Opcodes.ACC_PROTECTED) != 0 then
          if !name.startsWith("$") then {
            sb.append(":method:")
            sb.append(name)
            sb.append(":")
            sb.append(descriptor)
          }
        null
      }
    }

    reader.accept(visitor, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)

    computeHash(sb.toString)
  }

  /** Analyze a Kotlin JAR - uses kotlin-metadata-jvm for rich type information */
  private def analyzeKotlinJar(
      jarPath: Path,
      jar: JarFile,
      symbols: Set[Symbol],
      hashes: mutable.Map[Symbol, TypeHash]
  ): Unit = {
    val entries = jar.entries().asScala.toList
    val classEntries = entries.filter(e => e.getName.endsWith(".class") && !e.getName.contains("$"))

    for entry <- classEntries do
      try {
        val is = jar.getInputStream(entry)
        try {
          val classBytes = is.readAllBytes()
          val reader = new ClassReader(classBytes)
          val className = reader.getClassName.replace('/', '.')
          val sym = Symbol(className)

          // Skip if we have specific symbols and this isn't one of them
          if symbols.nonEmpty && !symbols.contains(sym) then () // skip
          else {
            // Hash with rich Kotlin metadata
            val hash = hashKotlinClassFile(classBytes, reader)
            hashes(sym) = TypeHash(sym, hash, jarPath)
          }
        } finally is.close()
      } catch {
        case _: Exception => () // Skip unreadable classes
      }
  }

  /** Extract Kotlin metadata from class bytes using ASM */
  private def extractKotlinMetadata(classBytes: Array[Byte], reader: ClassReader): Option[kotlin.Metadata] = {
    var kind: Int = 0
    var metadataVersion: Array[Int] = Array.empty
    var data1: Array[String] = Array.empty
    var data2: Array[String] = Array.empty
    var extraString: String = ""
    var packageName: String = ""
    var extraInt: Int = 0
    var hasMetadata = false

    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitAnnotation(descriptor: String, visible: Boolean): AnnotationVisitor =
        if descriptor == "Lkotlin/Metadata;" then {
          hasMetadata = true
          new AnnotationVisitor(Opcodes.ASM9) {
            override def visit(name: String, value: Any): Unit =
              name match {
                case "k"  => kind = value.asInstanceOf[Int]
                case "xi" => extraInt = value.asInstanceOf[Int]
                case "xs" => extraString = if value != null then value.asInstanceOf[String] else ""
                case "pn" => packageName = if value != null then value.asInstanceOf[String] else ""
                case "mv" => metadataVersion = value.asInstanceOf[Array[Int]] // mv is passed directly, not via visitArray
                case "bv" => () // bytecode version - ignore
                case _    => ()
              }

            override def visitArray(name: String): AnnotationVisitor =
              name match {
                case "d1" =>
                  new AnnotationVisitor(Opcodes.ASM9) {
                    private val values = mutable.ArrayBuffer[String]()
                    override def visit(n: String, v: Any): Unit =
                      values += v.asInstanceOf[String]
                    override def visitEnd(): Unit =
                      data1 = values.toArray
                  }
                case "d2" =>
                  new AnnotationVisitor(Opcodes.ASM9) {
                    private val values = mutable.ArrayBuffer[String]()
                    override def visit(n: String, v: Any): Unit =
                      values += v.asInstanceOf[String]
                    override def visitEnd(): Unit =
                      data2 = values.toArray
                  }
                case _ => null
              }
          }
        } else null
    }

    reader.accept(visitor, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)

    if hasMetadata && metadataVersion.nonEmpty then Some(JvmMetadataUtil.Metadata(kind, metadataVersion, data1, data2, extraString, packageName, extraInt))
    else None
  }

  /** Hash a Kotlin class file - uses kotlin-metadata-jvm for rich type information */
  private def hashKotlinClassFile(classBytes: Array[Byte], reader: ClassReader): String = {
    val sb = new StringBuilder
    sb.append("kotlin-class:")
    sb.append(reader.getClassName)

    // Try to extract and parse rich Kotlin metadata
    extractKotlinMetadata(classBytes, reader) match {
      case Some(metadata) =>
        try {
          val kcm = KotlinClassMetadata.Companion.readLenient(metadata)
          hashKotlinClassMetadata(kcm, sb)
        } catch {
          case _: Exception =>
            // Fall back to basic class file hashing if metadata parsing fails
            hashBasicClassFile(reader, sb)
        }
      case None =>
        // No Kotlin metadata, use basic class file hashing
        hashBasicClassFile(reader, sb)
    }

    computeHash(sb.toString)
  }

  /** Hash rich Kotlin metadata from kotlin-metadata-jvm */
  private def hashKotlinClassMetadata(kcm: KotlinClassMetadata, sb: StringBuilder): Unit =
    kcm match {
      case cls: KotlinClassMetadata.Class =>
        val kmClass = cls.getKmClass
        hashKmClass(kmClass, sb)

      case fileFacade: KotlinClassMetadata.FileFacade =>
        sb.append(":file-facade")
        val kmPackage = fileFacade.getKmPackage
        // Hash top-level functions
        kmPackage.getFunctions.asScala.toSeq.sortBy(_.getName).foreach { func =>
          hashKmFunction(func, sb)
        }
        // Hash top-level properties
        kmPackage.getProperties.asScala.toSeq.sortBy(_.getName).foreach { prop =>
          hashKmProperty(prop, sb)
        }
        // Hash type aliases
        kmPackage.getTypeAliases.asScala.toSeq.sortBy(_.getName).foreach { ta =>
          sb.append(":typealias:")
          sb.append(ta.getName)
          sb.append(":")
          hashKmType(ta.getUnderlyingType, sb)
        }

      case multiFileClassFacade: KotlinClassMetadata.MultiFileClassFacade =>
        sb.append(":multi-file-facade")
        multiFileClassFacade.getPartClassNames.asScala.sorted.foreach { part =>
          sb.append(":part:")
          sb.append(part)
        }

      case multiFileClassPart: KotlinClassMetadata.MultiFileClassPart =>
        sb.append(":multi-file-part:")
        sb.append(multiFileClassPart.getFacadeClassName)
        val kmPackage = multiFileClassPart.getKmPackage
        kmPackage.getFunctions.asScala.toSeq.sortBy(_.getName).foreach { func =>
          hashKmFunction(func, sb)
        }
        kmPackage.getProperties.asScala.toSeq.sortBy(_.getName).foreach { prop =>
          hashKmProperty(prop, sb)
        }

      case syntheticClass: KotlinClassMetadata.SyntheticClass =>
        sb.append(":synthetic")
        // Synthetic classes (like lambdas) may have kmLambda
        val kmLambda = syntheticClass.getKmLambda
        if kmLambda != null then {
          val func = kmLambda.getFunction
          if func != null then hashKmFunction(func, sb)
        }

      case _ =>
        sb.append(":unknown-kind")
    }

  /** Hash a Kotlin class (KmClass) */
  private def hashKmClass(kmClass: KmClass, sb: StringBuilder): Unit = {
    sb.append(":class:")
    sb.append(kmClass.getName)

    // Hash visibility
    val visibility = Attributes.getVisibility(kmClass)
    sb.append(":visibility:")
    sb.append(visibility.toString)

    // Hash modality (final, open, abstract, sealed)
    val modality = Attributes.getModality(kmClass)
    sb.append(":modality:")
    sb.append(modality.toString)

    // Hash class kind (class, interface, enum, annotation, object)
    val kind = Attributes.getKind(kmClass)
    sb.append(":kind:")
    sb.append(kind.toString)

    // Hash supertypes
    kmClass.getSupertypes.asScala.foreach { st =>
      sb.append(":supertype:")
      hashKmType(st, sb)
    }

    // Hash type parameters
    kmClass.getTypeParameters.asScala.foreach { tp =>
      sb.append(":tparam:")
      sb.append(tp.getName)
      sb.append(":")
      sb.append(tp.getVariance.toString)
    }

    // Hash constructors (sorted for determinism)
    kmClass.getConstructors.asScala.toSeq.sortBy(c => c.getValueParameters.asScala.map(_.getName).mkString(",")).foreach { ctor =>
      val ctorVisibility = Attributes.getVisibility(ctor)
      if ctorVisibility.toString == "PUBLIC" || ctorVisibility.toString == "PROTECTED" then {
        sb.append(":constructor:")
        sb.append(ctorVisibility.toString)
        ctor.getValueParameters.asScala.foreach { param =>
          sb.append(":")
          sb.append(param.getName)
          sb.append(":")
          hashKmType(param.getType, sb)
        }
      }
    }

    // Hash functions (public API only, sorted by name)
    kmClass.getFunctions.asScala.toSeq.sortBy(_.getName).foreach { func =>
      val funcVisibility = Attributes.getVisibility(func)
      if funcVisibility.toString == "PUBLIC" || funcVisibility.toString == "PROTECTED" then hashKmFunction(func, sb)
    }

    // Hash properties (public API only, sorted by name)
    kmClass.getProperties.asScala.toSeq.sortBy(_.getName).foreach { prop =>
      val propVisibility = Attributes.getVisibility(prop)
      if propVisibility.toString == "PUBLIC" || propVisibility.toString == "PROTECTED" then hashKmProperty(prop, sb)
    }

    // Hash nested class names
    kmClass.getNestedClasses.asScala.sorted.foreach { nested =>
      sb.append(":nested:")
      sb.append(nested)
    }

    // Hash type aliases in this class
    kmClass.getTypeAliases.asScala.toSeq.sortBy(_.getName).foreach { ta =>
      sb.append(":typealias:")
      sb.append(ta.getName)
      sb.append(":")
      hashKmType(ta.getUnderlyingType, sb)
    }
  }

  /** Hash a Kotlin function */
  private def hashKmFunction(func: KmFunction, sb: StringBuilder): Unit = {
    sb.append(":func:")
    sb.append(func.getName)

    // Visibility
    val visibility = Attributes.getVisibility(func)
    sb.append(":vis:")
    sb.append(visibility.toString)

    // Modality
    val modality = Attributes.getModality(func)
    sb.append(":mod:")
    sb.append(modality.toString)

    // Suspend modifier (important for coroutines)
    if Attributes.isSuspend(func) then sb.append(":suspend")

    // Inline modifier
    if Attributes.isInline(func) then sb.append(":inline")

    // Operator modifier
    if Attributes.isOperator(func) then sb.append(":operator")

    // Infix modifier
    if Attributes.isInfix(func) then sb.append(":infix")

    // Type parameters
    func.getTypeParameters.asScala.foreach { tp =>
      sb.append(":ftparam:")
      sb.append(tp.getName)
    }

    // Receiver type (extension function)
    val receiverType = func.getReceiverParameterType
    if receiverType != null then {
      sb.append(":receiver:")
      hashKmType(receiverType, sb)
    }

    // Value parameters
    func.getValueParameters.asScala.foreach { param =>
      sb.append(":param:")
      sb.append(param.getName)
      sb.append(":")
      hashKmType(param.getType, sb)
      // Include crossinline/noinline modifiers if present
      if Attributes.isCrossinline(param) then sb.append(":crossinline")
      if Attributes.isNoinline(param) then sb.append(":noinline")
    }

    // Return type
    sb.append(":ret:")
    hashKmType(func.getReturnType, sb)
  }

  /** Hash a Kotlin property */
  private def hashKmProperty(prop: KmProperty, sb: StringBuilder): Unit = {
    sb.append(":prop:")
    sb.append(prop.getName)

    // Visibility
    val visibility = Attributes.getVisibility(prop)
    sb.append(":vis:")
    sb.append(visibility.toString)

    // Modality
    val modality = Attributes.getModality(prop)
    sb.append(":mod:")
    sb.append(modality.toString)

    // Var/Val
    if Attributes.isVar(prop) then sb.append(":var")
    else sb.append(":val")

    // Const modifier
    if Attributes.isConst(prop) then sb.append(":const")

    // Late init
    if Attributes.isLateinit(prop) then sb.append(":lateinit")

    // Receiver type (extension property)
    val receiverType = prop.getReceiverParameterType
    if receiverType != null then {
      sb.append(":receiver:")
      hashKmType(receiverType, sb)
    }

    // Return type
    sb.append(":type:")
    hashKmType(prop.getReturnType, sb)
  }

  /** Hash a Kotlin type */
  private def hashKmType(kmType: KmType, sb: StringBuilder): Unit = {
    if kmType == null then {
      sb.append("null")
      return
    }

    // Nullability (important for Kotlin type system)
    if Attributes.isNullable(kmType) then sb.append("?")

    // Classifier
    val classifier = kmType.getClassifier
    classifier match {
      case cls: KmClassifier.Class =>
        sb.append(cls.getName)
      case tp: KmClassifier.TypeParameter =>
        sb.append("T")
        sb.append(tp.getId)
      case ta: KmClassifier.TypeAlias =>
        sb.append("alias:")
        sb.append(ta.getName)
    }

    // Type arguments
    val args = kmType.getArguments
    if args != null && !args.isEmpty then {
      sb.append("<")
      var first = true
      args.asScala.foreach { arg =>
        if !first then sb.append(",")
        first = false
        val argType = arg.getType
        if argType != null then hashKmType(argType, sb)
        else sb.append("*") // Star projection
      }
      sb.append(">")
    }
  }

  /** Basic class file hashing fallback (for non-Kotlin or failed metadata) */
  private def hashBasicClassFile(reader: ClassReader, sb: StringBuilder): Unit = {
    // Superclass
    if reader.getSuperName != null then {
      sb.append(":super:")
      sb.append(reader.getSuperName)
    }

    // Interfaces
    for iface <- reader.getInterfaces.sorted do {
      sb.append(":implements:")
      sb.append(iface)
    }

    // Visit class to extract public API members
    val memberVisitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitField(
          access: Int,
          name: String,
          descriptor: String,
          signature: String,
          value: Any
      ): org.objectweb.asm.FieldVisitor = {
        if (access & Opcodes.ACC_PUBLIC) != 0 || (access & Opcodes.ACC_PROTECTED) != 0 then {
          sb.append(":field:")
          sb.append(name)
          sb.append(":")
          sb.append(descriptor)
          if signature != null then {
            sb.append(":")
            sb.append(signature)
          }
        }
        null
      }

      override def visitMethod(
          access: Int,
          name: String,
          descriptor: String,
          signature: String,
          exceptions: Array[String]
      ): org.objectweb.asm.MethodVisitor = {
        if (access & Opcodes.ACC_PUBLIC) != 0 || (access & Opcodes.ACC_PROTECTED) != 0 then
          if !name.startsWith("$") then {
            sb.append(":method:")
            sb.append(name)
            sb.append(":")
            sb.append(descriptor)
            if signature != null then {
              sb.append(":")
              sb.append(signature)
            }
          }
        null
      }
    }

    reader.accept(memberVisitor, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
  }

  private def computeHash(content: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(content.getBytes("UTF-8"))
    md.digest().take(8).map("%02x".format(_)).mkString // Short hash for readability
  }
}
