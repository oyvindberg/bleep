
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForParsers extends BleepCodegenScript("GenerateForParsers") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set("parsers@jvm3").contains(target.project.value)) {
        val to = target.sources.resolve("generated/TreeLifts.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = new String(s"""|
      |package scala.meta
      |package internal
      |package quasiquotes
      |
      |import scala.runtime.ScalaRunTime
      |import scala.quoted._
      |import scala.meta.{Tree => MetaTree}
      |import scala.meta.internal.trees.Quasi
      |import scala.meta.internal.parsers.Absolutize._
      |import scala.meta.internal.parsers.Messages
      |
      |import scala.collection.mutable
      |import scala.annotation.tailrec
      |import scala.meta.trees.Origin
      |
      |
      |trait TreeLiftsTrait(using val internalQuotes: Quotes) {
      |  def liftTree(tree: MetaTree): internalQuotes.reflect.Tree
      |  def liftOptionTree[T: Type](maybeTree: Option[MetaTree]): internalQuotes.reflect.Tree
      |  def liftTrees[T: Type](trees: Seq[MetaTree]): internalQuotes.reflect.Tree
      |  def liftTreess(treess: List[List[MetaTree]]): internalQuotes.reflect.Tree
      |  def liftQuasi0(quasi: Quasi, optional: Boolean = false): internalQuotes.reflect.Tree
      |  def liftOrigin(origin: Origin): internalQuotes.reflect.Tree
      |
      |  protected def unquotesName(q: scala.meta.internal.trees.Quasi): Boolean
      |}
      |
      |
      |trait TreeLifts(using override val internalQuotes: Quotes)(isPatternMode: Boolean, dialectExpr: Expr[Dialect]) extends ReificationMacros with TreeLiftsTrait {
      |  import internalQuotes.reflect._
      |
      |  def treeByMode[T](expr: Expr[T], origin: Option[Tree], args: Tree*): Tree =
      |    val term = expr.asTerm match
      |      case Inlined(_, _, inlined) => inlined
      |    def getResultType(x: TypeRepr, hasImplicit: Boolean): (TypeRepr, Boolean) = x match
      |      case mType @ MethodType(_, _, res) => getResultType(res, hasImplicit || mType.isImplicit)
      |      case res => (res, hasImplicit)
      |    val apply = term.symbol.methodMember("apply").head
      |    val (resType, hasImplicit) = getResultType(expr.asTerm.tpe.memberType(apply), hasImplicit = false)
      |    resType.asType match
      |      case '[t] =>
      |        if isPatternMode then
      |          TypedOrTest(Unapply(Select.unique(term, "unapply"), Nil, args.toList), TypeTree.of[t])
      |        else 
      |          val applied = Select.overloaded(term, "apply", Nil, (origin.map(List(_)).getOrElse(Nil) ++ args.toList).asInstanceOf[List[Term]])
      |          applied.tpe match
      |            case MethodType(_, _, _) => Apply(applied, List(dialectExpr.asTerm))
      |            case _ => applied
      |
      |
      |  def term[T <: MetaTree](tree: T): Tree = liftableSubTree0(tree)
      |  def term[T <: MetaTree: Type](tree: Seq[T]): Tree = liftTrees[T](tree)
      |  def term[T <: MetaTree: Type](tree: List[T]): Tree = liftTrees[T](tree)
      |  @scala.annotation.targetName("term2") def term[T <: MetaTree: Type](tree: Seq[List[T]]): Tree = liftTreess(tree.toList)
      |  @scala.annotation.targetName("term3") def term[T <: MetaTree: Type](tree: List[List[T]]): Tree = liftTreess(tree)
      |  def term[T <: MetaTree: Type](tree: Option[T]): Tree = liftOptionTree[T](tree)
      |  def term(tree: Origin): Option[Tree] = if (isPatternMode) None else Some(liftOrigin(tree))
      |
      |  def term(tree: String): Tree = Literal(StringConstant(tree))
      |  def term(tree: Byte): Tree = Literal(ByteConstant(tree))
      |  def term(tree: Int): Tree = Literal(IntConstant(tree))
      |  def term(tree: Long): Tree = Literal(LongConstant(tree))
      |  def term(tree: Boolean): Tree = Literal(BooleanConstant(tree))
      |  def term(tree: scala.Symbol): Tree = '{scala.Symbol($${Expr(tree.name)})}.asTerm//Apply(Literal(StringConstant(tree)) 
      |
      |
      |  def liftQuasi(x$$macro$$3: scala.meta.internal.trees.Quasi) = liftQuasi0(x$$macro$$3)
      |  def liftTypeFuncParamClause(x$$macro$$3: scala.meta.Type.FuncParamClause) = treeByMode('{_root_.scala.meta.Type.FuncParamClause.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.values))
      |  def liftTypeBounds(x$$macro$$3: scala.meta.Type.Bounds) = treeByMode('{_root_.scala.meta.Type.Bounds.After_4_12_3}, term(x$$macro$$3.origin), term(x$$macro$$3.lo), term(x$$macro$$3.hi), term(x$$macro$$3.context), term(x$$macro$$3.view))
      |  def liftMemberParamClauseGroup(x$$macro$$3: scala.meta.Member.ParamClauseGroup) = treeByMode('{_root_.scala.meta.Member.ParamClauseGroup.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tparamClause), term(x$$macro$$3.paramClauses))
      |  def liftTemplate(x$$macro$$3: scala.meta.Template) = treeByMode('{_root_.scala.meta.Template.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.earlyClause), term(x$$macro$$3.inits), term(x$$macro$$3.body), term(x$$macro$$3.derives))
      |  def liftImporter(x$$macro$$3: scala.meta.Importer) = treeByMode('{_root_.scala.meta.Importer.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.ref), term(x$$macro$$3.importees))
      |  def liftSource(x$$macro$$3: scala.meta.Source) = treeByMode('{_root_.scala.meta.Source.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.stats))
      |  def liftMultiSource(x$$macro$$3: scala.meta.MultiSource) = treeByMode('{_root_.scala.meta.MultiSource.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.sources))
      |  def liftStatBlock(x$$macro$$3: scala.meta.Stat.Block) = treeByMode('{_root_.scala.meta.Stat.Block.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.stats))
      |  def liftTermEnumeratorsBlock(x$$macro$$3: scala.meta.Term.EnumeratorsBlock) = treeByMode('{_root_.scala.meta.Term.EnumeratorsBlock.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.enums))
      |  def li""".stripMargin) + new String(s"""|ftTermBlock(x$$macro$$3: scala.meta.Term.Block) = treeByMode('{_root_.scala.meta.Term.Block.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.stats))
      |  def liftTypeBlock(x$$macro$$3: scala.meta.Type.Block) = treeByMode('{_root_.scala.meta.Type.Block.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.typeDefs), term(x$$macro$$3.tpe))
      |  def liftPkgBody(x$$macro$$3: scala.meta.Pkg.Body) = treeByMode('{_root_.scala.meta.Pkg.Body.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.stats))
      |  def liftCtorBlock(x$$macro$$3: scala.meta.Ctor.Block) = treeByMode('{_root_.scala.meta.Ctor.Block.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.init), term(x$$macro$$3.stats))
      |  def liftTemplateBody(x$$macro$$3: scala.meta.Template.Body) = treeByMode('{_root_.scala.meta.Template.Body.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.selfOpt), term(x$$macro$$3.stats))
      |  def liftTermCasesBlock(x$$macro$$3: scala.meta.Term.CasesBlock) = treeByMode('{_root_.scala.meta.Term.CasesBlock.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.cases))
      |  def liftTermPartialFunction(x$$macro$$3: scala.meta.Term.PartialFunction) = treeByMode('{_root_.scala.meta.Term.PartialFunction.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.cases))
      |  def liftTypeCasesBlock(x$$macro$$3: scala.meta.Type.CasesBlock) = treeByMode('{_root_.scala.meta.Type.CasesBlock.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.cases))
      |  def liftInit(x$$macro$$3: scala.meta.Init) = treeByMode('{_root_.scala.meta.Init.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe), term(x$$macro$$3.name), term(x$$macro$$3.argClauses))
      |  def liftNameAnonymous(x$$macro$$3: scala.meta.Name.Anonymous) = treeByMode('{_root_.scala.meta.Name.Anonymous.Initial}, term(x$$macro$$3.origin))
      |  def liftNameThis(x$$macro$$3: scala.meta.Name.This) = treeByMode('{_root_.scala.meta.Name.This.Initial}, term(x$$macro$$3.origin))
      |  def liftNameIndeterminate(x$$macro$$3: scala.meta.Name.Indeterminate) = treeByMode('{_root_.scala.meta.Name.Indeterminate.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftNamePlaceholder(x$$macro$$3: scala.meta.Name.Placeholder) = treeByMode('{_root_.scala.meta.Name.Placeholder.Initial}, term(x$$macro$$3.origin))
      |  def liftTermName(x$$macro$$3: scala.meta.Term.Name) = treeByMode('{_root_.scala.meta.Term.Name.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftTermCapSetName(x$$macro$$3: scala.meta.Term.CapSetName) = treeByMode('{_root_.scala.meta.Term.CapSetName.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftTermAnonymous(x$$macro$$3: scala.meta.Term.Anonymous) = treeByMode('{_root_.scala.meta.Term.Anonymous.Initial}, term(x$$macro$$3.origin))
      |  def liftTypeName(x$$macro$$3: scala.meta.Type.Name) = treeByMode('{_root_.scala.meta.Type.Name.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftTypeCapSetName(x$$macro$$3: scala.meta.Type.CapSetName) = treeByMode('{_root_.scala.meta.Type.CapSetName.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftTermThis(x$$macro$$3: scala.meta.Term.This) = treeByMode('{_root_.scala.meta.Term.This.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.qual))
      |  def liftTermSuper(x$$macro$$3: scala.meta.Term.Super) = treeByMode('{_root_.scala.meta.Term.Super.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.thisp), term(x$$macro$$3.superp))
      |  def liftTermApplyUnary(x$$macro$$3: scala.meta.Term.ApplyUnary) = treeByMode('{_root_.scala.meta.Term.ApplyUnary.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.op), term(x$$macro$$3.arg))
      |  def liftTermSelect(x$$macro$$3: scala.meta.Term.Select) = treeByMode('{_root_.scala.meta.Term.Select.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.qual), term(x$$macro$$3.name))
      |  def liftTermSelectPostfix(x$$macro$$3: scala.meta.Term.SelectPostfix) = treeByMode('{_root_.scala.meta.Term.SelectPostfix.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.qual), term(x$$macro$$3.name))
      |  def liftTypeSelect(x$$macro$$3: scala.meta.Type.Select) = treeByMode('{_root_.scala.meta.Type.Select.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.qual), term(x$$macro$$3.name))
      |  def liftTypeProject(x$$macro$$3: scala.meta.Type.Project) = treeByMode('{_root_.scala.meta.Type.Project.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.qual), term(x$$macro$$3.name))
      |  def liftTypeSingleton(x$$macro$$3: scala.meta.Type.Singleton) = treeByMode('{_root_.scala.meta.Type.Singleton.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.ref))
      |  def liftTypeBoundsAlias(x$$macro$$3: scala.meta.Type.BoundsAlias) = treeByMode('{_root_.scala.meta.Type.BoundsAlias.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name), term(x$$macro$$3.bounds))
      |  def liftImporteeWildcard(x$$macro$$3: scala.meta.Importee.Wildcard) = treeByMode('{_root_.scala.meta.Importee.Wildcard.Initial}, term(x$$macro$$3.origin))
      |  def liftImporteeGiven(x$$macro$$3: scala.meta.Importee.Given) = treeByMode('{_root_.scala.meta.Importee.Given.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe))
      |  def liftImporteeGivenAll(x$$macro$$3: scala.meta.Importee.GivenAll) = treeByMode('{_root_.scala.meta.Importee.GivenAl""".stripMargin) + new String(s"""|l.Initial}, term(x$$macro$$3.origin))
      |  def liftImporteeName(x$$macro$$3: scala.meta.Importee.Name) = treeByMode('{_root_.scala.meta.Importee.Name.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name))
      |  def liftImporteeRename(x$$macro$$3: scala.meta.Importee.Rename) = treeByMode('{_root_.scala.meta.Importee.Rename.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name), term(x$$macro$$3.rename))
      |  def liftImporteeUnimport(x$$macro$$3: scala.meta.Importee.Unimport) = treeByMode('{_root_.scala.meta.Importee.Unimport.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name))
      |  def liftTypeAnonymousName(x$$macro$$3: scala.meta.Type.AnonymousName) = treeByMode('{_root_.scala.meta.Type.AnonymousName.Initial}, term(x$$macro$$3.origin))
      |  def liftTypeApply(x$$macro$$3: scala.meta.Type.Apply) = treeByMode('{_root_.scala.meta.Type.Apply.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe), term(x$$macro$$3.argClause))
      |  def liftTypeApplyInfix(x$$macro$$3: scala.meta.Type.ApplyInfix) = treeByMode('{_root_.scala.meta.Type.ApplyInfix.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.op), term(x$$macro$$3.rhs))
      |  def liftTypePolyFunction(x$$macro$$3: scala.meta.Type.PolyFunction) = treeByMode('{_root_.scala.meta.Type.PolyFunction.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.tparamClause), term(x$$macro$$3.tpe))
      |  def liftTypeImplicitFunction(x$$macro$$3: scala.meta.Type.ImplicitFunction) = treeByMode('{_root_.scala.meta.Type.ImplicitFunction.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.params), term(x$$macro$$3.res))
      |  def liftTypeTuple(x$$macro$$3: scala.meta.Type.Tuple) = treeByMode('{_root_.scala.meta.Type.Tuple.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.args))
      |  def liftTypeWith(x$$macro$$3: scala.meta.Type.With) = treeByMode('{_root_.scala.meta.Type.With.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.rhs))
      |  def liftTypeAnd(x$$macro$$3: scala.meta.Type.And) = treeByMode('{_root_.scala.meta.Type.And.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.rhs))
      |  def liftTypeOr(x$$macro$$3: scala.meta.Type.Or) = treeByMode('{_root_.scala.meta.Type.Or.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.rhs))
      |  def liftTypeRefine(x$$macro$$3: scala.meta.Type.Refine) = treeByMode('{_root_.scala.meta.Type.Refine.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe), term(x$$macro$$3.body))
      |  def liftTypeExistential(x$$macro$$3: scala.meta.Type.Existential) = treeByMode('{_root_.scala.meta.Type.Existential.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe), term(x$$macro$$3.body))
      |  def liftTypeAnnotate(x$$macro$$3: scala.meta.Type.Annotate) = treeByMode('{_root_.scala.meta.Type.Annotate.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe), term(x$$macro$$3.annots))
      |  def liftTypeLambda(x$$macro$$3: scala.meta.Type.Lambda) = treeByMode('{_root_.scala.meta.Type.Lambda.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.tparamClause), term(x$$macro$$3.tpe))
      |  def liftTypeAnonymousLambda(x$$macro$$3: scala.meta.Type.AnonymousLambda) = treeByMode('{_root_.scala.meta.Type.AnonymousLambda.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe))
      |  def liftTypeMacro(x$$macro$$3: scala.meta.Type.Macro) = treeByMode('{_root_.scala.meta.Type.Macro.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.body))
      |  def liftTypeMethod(x$$macro$$3: scala.meta.Type.Method) = treeByMode('{_root_.scala.meta.Type.Method.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.paramClauses), term(x$$macro$$3.tpe))
      |  def liftTypePatWildcard(x$$macro$$3: scala.meta.Type.PatWildcard) = treeByMode('{_root_.scala.meta.Type.PatWildcard.Initial}, term(x$$macro$$3.origin))
      |  def liftTypeRepeated(x$$macro$$3: scala.meta.Type.Repeated) = treeByMode('{_root_.scala.meta.Type.Repeated.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe))
      |  def liftTypeVar(x$$macro$$3: scala.meta.Type.Var) = treeByMode('{_root_.scala.meta.Type.Var.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name))
      |  def liftTypeAssign(x$$macro$$3: scala.meta.Type.Assign) = treeByMode('{_root_.scala.meta.Type.Assign.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name), term(x$$macro$$3.rhs))
      |  def liftTypeMatch(x$$macro$$3: scala.meta.Type.Match) = treeByMode('{_root_.scala.meta.Type.Match.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe), term(x$$macro$$3.casesBlock))
      |  def liftTypeCapturing(x$$macro$$3: scala.meta.Type.Capturing) = treeByMode('{_root_.scala.meta.Type.Capturing.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe), term(x$$macro$$3.caps))
      |  def liftLitNull(x$$macro$$3: scala.meta.Lit.Null) = treeByMode('{_root_.scala.meta.Lit.Null.Initial}, term(x$$macro$$3.origin))
      |  def liftLitInt(x$$macro$$3: scala.meta.Lit.Int) = treeByMode('{_root_.scala.meta.Lit.Int.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftLitDouble(x$$macro$$3: scala.meta.Lit.Double) = treeByMode('{_root_.scala.meta.Lit.Double.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.format))
      |  def liftLitFloat(x$$macro$$3: scala.meta.Lit.Float) = treeByMode('{_root_.scala.meta.Lit""".stripMargin) + new String(s"""|.Float.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.format))
      |  def liftLitByte(x$$macro$$3: scala.meta.Lit.Byte) = treeByMode('{_root_.scala.meta.Lit.Byte.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftLitShort(x$$macro$$3: scala.meta.Lit.Short) = treeByMode('{_root_.scala.meta.Lit.Short.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftLitChar(x$$macro$$3: scala.meta.Lit.Char) = treeByMode('{_root_.scala.meta.Lit.Char.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftLitLong(x$$macro$$3: scala.meta.Lit.Long) = treeByMode('{_root_.scala.meta.Lit.Long.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftLitBoolean(x$$macro$$3: scala.meta.Lit.Boolean) = treeByMode('{_root_.scala.meta.Lit.Boolean.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftLitUnit(x$$macro$$3: scala.meta.Lit.Unit) = treeByMode('{_root_.scala.meta.Lit.Unit.Initial}, term(x$$macro$$3.origin))
      |  def liftLitString(x$$macro$$3: scala.meta.Lit.String) = treeByMode('{_root_.scala.meta.Lit.String.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftLitSymbol(x$$macro$$3: scala.meta.Lit.Symbol) = treeByMode('{_root_.scala.meta.Lit.Symbol.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.value))
      |  def liftTypePureFunction(x$$macro$$3: scala.meta.Type.PureFunction) = treeByMode('{_root_.scala.meta.Type.PureFunction.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.paramClause), term(x$$macro$$3.res))
      |  def liftTypePureContextFunction(x$$macro$$3: scala.meta.Type.PureContextFunction) = treeByMode('{_root_.scala.meta.Type.PureContextFunction.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.paramClause), term(x$$macro$$3.res))
      |  def liftTypeFunction(x$$macro$$3: scala.meta.Type.Function) = treeByMode('{_root_.scala.meta.Type.Function.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.paramClause), term(x$$macro$$3.res))
      |  def liftTypeContextFunction(x$$macro$$3: scala.meta.Type.ContextFunction) = treeByMode('{_root_.scala.meta.Type.ContextFunction.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.paramClause), term(x$$macro$$3.res))
      |  def liftTypeByName(x$$macro$$3: scala.meta.Type.ByName) = treeByMode('{_root_.scala.meta.Type.ByName.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe))
      |  def liftTypePureByName(x$$macro$$3: scala.meta.Type.PureByName) = treeByMode('{_root_.scala.meta.Type.PureByName.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe))
      |  def liftTypePlaceholderImpl(x$$macro$$3: scala.meta.Type.Placeholder.Impl) = treeByMode('{_root_.scala.meta.Type.Placeholder.Impl.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.bounds))
      |  def liftTypeWildcard(x$$macro$$3: scala.meta.Type.Wildcard) = treeByMode('{_root_.scala.meta.Type.Wildcard.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.bounds))
      |  def liftTypeAnonymousParam(x$$macro$$3: scala.meta.Type.AnonymousParam) = treeByMode('{_root_.scala.meta.Type.AnonymousParam.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.variant))
      |  def liftTypeTypedParam(x$$macro$$3: scala.meta.Type.TypedParam) = treeByMode('{_root_.scala.meta.Type.TypedParam.After_4_7_8}, term(x$$macro$$3.origin), term(x$$macro$$3.name), term(x$$macro$$3.typ), term(x$$macro$$3.mods))
      |  def liftTypeFunctionArg(x$$macro$$3: scala.meta.Type.FunctionArg) = treeByMode('{_root_.scala.meta.Type.FunctionArg.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.tpe))
      |  def liftPatVar(x$$macro$$3: scala.meta.Pat.Var) = treeByMode('{_root_.scala.meta.Pat.Var.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name))
      |  def liftPatWildcard(x$$macro$$3: scala.meta.Pat.Wildcard) = treeByMode('{_root_.scala.meta.Pat.Wildcard.Initial}, term(x$$macro$$3.origin))
      |  def liftPatSeqWildcard(x$$macro$$3: scala.meta.Pat.SeqWildcard) = treeByMode('{_root_.scala.meta.Pat.SeqWildcard.Initial}, term(x$$macro$$3.origin))
      |  def liftPatBind(x$$macro$$3: scala.meta.Pat.Bind) = { 
      |    import internalQuotes.reflect._
      |    def prohibitName(pat: _root_.scala.meta.Tree): _root_.scala.Unit = {
      |      pat match {
      |        case q: _root_.scala.meta.internal.trees.Quasi if unquotesName(q) =>
      |          val action = if (q.rank == 0) "unquote" else "splice"
      |          report.errorAndAbort("can't " + action + " a name here, use a pattern instead (e.g. p\\\\"x\\\\")")
      |        case _ =>
      |      }
      |    }
      |    prohibitName(x$$macro$$3.lhs)
      |
      |    treeByMode('{_root_.scala.meta.Pat.Bind.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.rhs)) }
      |  def liftPatAlternative(x$$macro$$3: scala.meta.Pat.Alternative) = treeByMode('{_root_.scala.meta.Pat.Alternative.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.rhs))
      |  def liftPatTuple(x$$macro$$3: scala.meta.Pat.Tuple) = treeByMode('{_root_.scala.meta.Pat.Tuple.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.args))
      |  def liftPatRepeated(x$$macro$$3: scala.meta.Pat.Repeated) = treeByMode('{_root_.scala.meta.Pat.Repeated.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name))
      |  def liftPatExtract(x$$macro$$3: scala.meta.Pat.Extract) = treeByMode('{_root_.scala.""".stripMargin) + new String(s"""|meta.Pat.Extract.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.fun), term(x$$macro$$3.argClause))
      |  def liftPatExtractInfix(x$$macro$$3: scala.meta.Pat.ExtractInfix) = treeByMode('{_root_.scala.meta.Pat.ExtractInfix.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.op), term(x$$macro$$3.argClause))
      |  def liftPatInterpolate(x$$macro$$3: scala.meta.Pat.Interpolate) = treeByMode('{_root_.scala.meta.Pat.Interpolate.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.prefix), term(x$$macro$$3.parts), term(x$$macro$$3.args))
      |  def liftPatXml(x$$macro$$3: scala.meta.Pat.Xml) = treeByMode('{_root_.scala.meta.Pat.Xml.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.parts), term(x$$macro$$3.args))
      |  def liftPatTyped(x$$macro$$3: scala.meta.Pat.Typed) = { 
      |    import internalQuotes.reflect._
      |    def prohibitName(pat: _root_.scala.meta.Tree): _root_.scala.Unit = {
      |      pat match {
      |        case q: _root_.scala.meta.internal.trees.Quasi if unquotesName(q) =>
      |          val action = if (q.rank == 0) "unquote" else "splice"
      |          report.errorAndAbort("can't " + action + " a name here, use a pattern instead (e.g. p\\\\"x\\\\")")
      |        case _ =>
      |      }
      |    }
      |    prohibitName(x$$macro$$3.lhs)
      |
      |    treeByMode('{_root_.scala.meta.Pat.Typed.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.rhs)) }
      |  def liftPatAssign(x$$macro$$3: scala.meta.Pat.Assign) = treeByMode('{_root_.scala.meta.Pat.Assign.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name), term(x$$macro$$3.rhs))
      |  def liftPatMacro(x$$macro$$3: scala.meta.Pat.Macro) = treeByMode('{_root_.scala.meta.Pat.Macro.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.body))
      |  def liftPatGiven(x$$macro$$3: scala.meta.Pat.Given) = treeByMode('{_root_.scala.meta.Pat.Given.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe))
      |  def liftPkg(x$$macro$$3: scala.meta.Pkg) = treeByMode('{_root_.scala.meta.Pkg.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.ref), term(x$$macro$$3.body))
      |  def liftPkgObject(x$$macro$$3: scala.meta.Pkg.Object) = treeByMode('{_root_.scala.meta.Pkg.Object.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.templ))
      |  def liftCtorSecondary(x$$macro$$3: scala.meta.Ctor.Secondary) = treeByMode('{_root_.scala.meta.Ctor.Secondary.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.paramClauses), term(x$$macro$$3.body))
      |  def liftTermInterpolate(x$$macro$$3: scala.meta.Term.Interpolate) = treeByMode('{_root_.scala.meta.Term.Interpolate.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.prefix), term(x$$macro$$3.parts), term(x$$macro$$3.args))
      |  def liftTermXml(x$$macro$$3: scala.meta.Term.Xml) = treeByMode('{_root_.scala.meta.Term.Xml.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.parts), term(x$$macro$$3.args))
      |  def liftTermApply(x$$macro$$3: scala.meta.Term.Apply) = 
      |    object ApplyToTripleDots extends scala.AnyRef {
      |      def unapply(t: scala.meta.Term.Apply): Option[scala.Tuple2[scala.meta.Term, scala.meta.Term.ArgClause.Quasi]] = t.argClause match {
      |        case (arg @ (_: scala.meta.Term.ArgClause.Quasi)) if arg.rank == 1 => scala.Some(scala.Tuple2(t.fun, arg))
      |        case _ => scala.None
      |      }
      |    }
      |    @tailrec() def checkNoTripleDots(fn: scala.meta.Term, arg: scala.meta.internal.trees.Quasi): Unit = fn match {
      |      case (t @ (_: scala.meta.Term.Apply)) => ApplyToTripleDots.unapply(t) match {
      |        case scala.None => checkNoTripleDots(t.fun, arg)
      |        case _ => report.errorAndAbort("rank mismatch when unquoting")
      |      }
      |      case _ => () // do nothing
      |    }
      |    def applyArgClauseQuasi(fn: _root_.scala.meta.Term)(arg: _root_.scala.meta.Term.ArgClause.Quasi) = {
      |      checkNoTripleDots(fn, arg)
      |      treeByMode('{scala.meta.internal.trees.Syntactic.TermApply.ArgList}, None, term(fn), term(List(arg)))
      |    }
      |    x$$macro$$3 match {
      |      case ApplyToTripleDots(fn, acq) => applyArgClauseQuasi(fn)(acq)
      |      case _ => treeByMode('{_root_.scala.meta.Term.Apply.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.fun), term(x$$macro$$3.argClause))
      |    }
      |
      |  def liftTermApplyUsing(x$$macro$$3: scala.meta.Term.ApplyUsing) = treeByMode('{_root_.scala.meta.Term.ApplyUsing.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.fun), term(x$$macro$$3.argClause))
      |  def liftTermApplyType(x$$macro$$3: scala.meta.Term.ApplyType) = treeByMode('{_root_.scala.meta.Term.ApplyType.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.fun), term(x$$macro$$3.targClause))
      |  def liftTermApplyInfix(x$$macro$$3: scala.meta.Term.ApplyInfix) = treeByMode('{_root_.scala.meta.Term.ApplyInfix.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.op), term(x$$macro$$3.targClause), term(x$$macro$$3.argClause))
      |  def liftTermAssign(x$$macro$$3: scala.meta.Term.Assign) = treeByMode('{_root_.scala.meta.Term.Assign.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.lhs), term(x$$macro$$3.rhs))
      |  def liftTermReturn(x$$macro$$3: scala.meta.Term.Return) = treeByMode('{_root_.scala.meta.Term.Return.Initi""".stripMargin) + new String(s"""|al}, term(x$$macro$$3.origin), term(x$$macro$$3.expr))
      |  def liftTermThrow(x$$macro$$3: scala.meta.Term.Throw) = treeByMode('{_root_.scala.meta.Term.Throw.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.expr))
      |  def liftTermAscribe(x$$macro$$3: scala.meta.Term.Ascribe) = treeByMode('{_root_.scala.meta.Term.Ascribe.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.expr), term(x$$macro$$3.tpe))
      |  def liftTermAnnotate(x$$macro$$3: scala.meta.Term.Annotate) = treeByMode('{_root_.scala.meta.Term.Annotate.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.expr), term(x$$macro$$3.annots))
      |  def liftTermTuple(x$$macro$$3: scala.meta.Term.Tuple) = treeByMode('{_root_.scala.meta.Term.Tuple.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.args))
      |  def liftTermEndMarker(x$$macro$$3: scala.meta.Term.EndMarker) = treeByMode('{_root_.scala.meta.Term.EndMarker.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name))
      |  def liftTermIf(x$$macro$$3: scala.meta.Term.If) = treeByMode('{_root_.scala.meta.Term.If.After_4_4_0}, term(x$$macro$$3.origin), term(x$$macro$$3.cond), term(x$$macro$$3.thenp), term(x$$macro$$3.elsep), term(x$$macro$$3.mods))
      |  def liftTermQuotedMacroExpr(x$$macro$$3: scala.meta.Term.QuotedMacroExpr) = treeByMode('{_root_.scala.meta.Term.QuotedMacroExpr.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.body))
      |  def liftTermQuotedMacroType(x$$macro$$3: scala.meta.Term.QuotedMacroType) = treeByMode('{_root_.scala.meta.Term.QuotedMacroType.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.tpe))
      |  def liftTermSplicedMacroExpr(x$$macro$$3: scala.meta.Term.SplicedMacroExpr) = treeByMode('{_root_.scala.meta.Term.SplicedMacroExpr.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.body))
      |  def liftTermSplicedMacroPat(x$$macro$$3: scala.meta.Term.SplicedMacroPat) = treeByMode('{_root_.scala.meta.Term.SplicedMacroPat.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.pat))
      |  def liftTermAnonymousFunction(x$$macro$$3: scala.meta.Term.AnonymousFunction) = treeByMode('{_root_.scala.meta.Term.AnonymousFunction.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.body))
      |  def liftTermPolyFunction(x$$macro$$3: scala.meta.Term.PolyFunction) = treeByMode('{_root_.scala.meta.Term.PolyFunction.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.tparamClause), term(x$$macro$$3.body))
      |  def liftTermWhile(x$$macro$$3: scala.meta.Term.While) = treeByMode('{_root_.scala.meta.Term.While.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.expr), term(x$$macro$$3.body))
      |  def liftTermDo(x$$macro$$3: scala.meta.Term.Do) = treeByMode('{_root_.scala.meta.Term.Do.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.body), term(x$$macro$$3.expr))
      |  def liftTermNew(x$$macro$$3: scala.meta.Term.New) = treeByMode('{_root_.scala.meta.Term.New.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.init))
      |  def liftTermNewAnonymous(x$$macro$$3: scala.meta.Term.NewAnonymous) = treeByMode('{_root_.scala.meta.Term.NewAnonymous.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.templ))
      |  def liftTermPlaceholder(x$$macro$$3: scala.meta.Term.Placeholder) = treeByMode('{_root_.scala.meta.Term.Placeholder.Initial}, term(x$$macro$$3.origin))
      |  def liftTermEta(x$$macro$$3: scala.meta.Term.Eta) = treeByMode('{_root_.scala.meta.Term.Eta.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.expr))
      |  def liftTermRepeated(x$$macro$$3: scala.meta.Term.Repeated) = treeByMode('{_root_.scala.meta.Term.Repeated.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.expr))
      |  def liftTermMatch(x$$macro$$3: scala.meta.Term.Match) = treeByMode('{_root_.scala.meta.Term.Match.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.expr), term(x$$macro$$3.casesBlock), term(x$$macro$$3.mods))
      |  def liftTermSelectMatch(x$$macro$$3: scala.meta.Term.SelectMatch) = treeByMode('{_root_.scala.meta.Term.SelectMatch.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.expr), term(x$$macro$$3.casesBlock), term(x$$macro$$3.mods))
      |  def liftTermTry(x$$macro$$3: scala.meta.Term.Try) = treeByMode('{_root_.scala.meta.Term.Try.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.expr), term(x$$macro$$3.catchClause), term(x$$macro$$3.finallyp))
      |  def liftTermTryWithHandler(x$$macro$$3: scala.meta.Term.TryWithHandler) = treeByMode('{_root_.scala.meta.Term.TryWithHandler.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.expr), term(x$$macro$$3.catchp), term(x$$macro$$3.finallyp))
      |  def liftTermContextFunction(x$$macro$$3: scala.meta.Term.ContextFunction) = treeByMode('{_root_.scala.meta.Term.ContextFunction.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.paramClause), term(x$$macro$$3.body))
      |  def liftTermFunction(x$$macro$$3: scala.meta.Term.Function) = treeByMode('{_root_.scala.meta.Term.Function.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.paramClause), term(x$$macro$$3.body))
      |  def liftTermFor(x$$macro$$3: scala.meta.Term.For) = treeByMode('{_root_.scala.meta.Term.For.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.enumsBlock), term(x$$macro$$3.body))
      |  def liftTermForYield(x$$macro$$3: scala.meta.Term.ForYield) = treeByMode('{_root_.scala.meta.Term.ForYield.After_4_9_9}, term(x$$macro$$3.origin), term(x$$macro$$3.enums""".stripMargin) + new String(s"""|Block), term(x$$macro$$3.body))
      |  def liftDeclVal(x$$macro$$3: scala.meta.Decl.Val) = treeByMode('{_root_.scala.meta.Decl.Val.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.pats), term(x$$macro$$3.decltpe))
      |  def liftDeclVar(x$$macro$$3: scala.meta.Decl.Var) = treeByMode('{_root_.scala.meta.Decl.Var.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.pats), term(x$$macro$$3.decltpe))
      |  def liftDeclDef(x$$macro$$3: scala.meta.Decl.Def) = treeByMode('{_root_.scala.meta.Decl.Def.After_4_7_3}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.paramClauseGroups), term(x$$macro$$3.decltpe))
      |  def liftDefnVal(x$$macro$$3: scala.meta.Defn.Val) = { x$$macro$$3.pats.foreach{pat => 
      |    import internalQuotes.reflect._
      |    def prohibitName(pat: _root_.scala.meta.Tree): _root_.scala.Unit = {
      |      pat match {
      |        case q: _root_.scala.meta.internal.trees.Quasi if unquotesName(q) =>
      |          val action = if (q.rank == 0) "unquote" else "splice"
      |          report.errorAndAbort("can't " + action + " a name here, use a pattern instead (e.g. p\\\\"x\\\\")")
      |        case _ =>
      |      }
      |    }
      |    prohibitName(pat)
      |}
      |    treeByMode('{_root_.scala.meta.Defn.Val.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.pats), term(x$$macro$$3.decltpe), term(x$$macro$$3.rhs)) }
      |  def liftDefnVar(x$$macro$$3: scala.meta.Defn.Var) = { x$$macro$$3.pats.foreach{pat => 
      |    import internalQuotes.reflect._
      |    def prohibitName(pat: _root_.scala.meta.Tree): _root_.scala.Unit = {
      |      pat match {
      |        case q: _root_.scala.meta.internal.trees.Quasi if unquotesName(q) =>
      |          val action = if (q.rank == 0) "unquote" else "splice"
      |          report.errorAndAbort("can't " + action + " a name here, use a pattern instead (e.g. p\\\\"x\\\\")")
      |        case _ =>
      |      }
      |    }
      |    prohibitName(pat)
      |}
      |    treeByMode('{_root_.scala.meta.Defn.Var.After_4_7_2}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.pats), term(x$$macro$$3.decltpe), term(x$$macro$$3.body)) }
      |  def liftDefnEnum(x$$macro$$3: scala.meta.Defn.Enum) = treeByMode('{_root_.scala.meta.Defn.Enum.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.tparamClause), term(x$$macro$$3.ctor), term(x$$macro$$3.templ))
      |  def liftDefnEnumCase(x$$macro$$3: scala.meta.Defn.EnumCase) = treeByMode('{_root_.scala.meta.Defn.EnumCase.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.tparamClause), term(x$$macro$$3.ctor), term(x$$macro$$3.inits))
      |  def liftDefnRepeatedEnumCase(x$$macro$$3: scala.meta.Defn.RepeatedEnumCase) = treeByMode('{_root_.scala.meta.Defn.RepeatedEnumCase.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.cases))
      |  def liftDefnDef(x$$macro$$3: scala.meta.Defn.Def) = treeByMode('{_root_.scala.meta.Defn.Def.After_4_7_3}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.paramClauseGroups), term(x$$macro$$3.decltpe), term(x$$macro$$3.body))
      |  def liftDefnMacro(x$$macro$$3: scala.meta.Defn.Macro) = treeByMode('{_root_.scala.meta.Defn.Macro.After_4_7_3}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.paramClauseGroups), term(x$$macro$$3.decltpe), term(x$$macro$$3.body))
      |  def liftDefnClass(x$$macro$$3: scala.meta.Defn.Class) = treeByMode('{_root_.scala.meta.Defn.Class.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.tparamClause), term(x$$macro$$3.ctor), term(x$$macro$$3.templ))
      |  def liftDefnTrait(x$$macro$$3: scala.meta.Defn.Trait) = treeByMode('{_root_.scala.meta.Defn.Trait.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.tparamClause), term(x$$macro$$3.ctor), term(x$$macro$$3.templ))
      |  def liftDefnObject(x$$macro$$3: scala.meta.Defn.Object) = treeByMode('{_root_.scala.meta.Defn.Object.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.templ))
      |  def liftDeclType(x$$macro$$3: scala.meta.Decl.Type) = treeByMode('{_root_.scala.meta.Decl.Type.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.tparamClause), term(x$$macro$$3.bounds))
      |  def liftDefnType(x$$macro$$3: scala.meta.Defn.Type) = treeByMode('{_root_.scala.meta.Defn.Type.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.tparamClause), term(x$$macro$$3.body), term(x$$macro$$3.bounds))
      |  def liftDefnGiven(x$$macro$$3: scala.meta.Defn.Given) = treeByMode('{_root_.scala.meta.Defn.Given.After_4_12_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.paramClauseGroups), term(x$$macro$$3.templ))
      |  def liftDefnGivenAlias(x$$macro$$3: scala.meta.Defn.GivenAlias) = treeByMode('{_root_.scala.meta.Defn.GivenAlias.After_4_12_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.paramClauseGroups), term(x$$macro$$3.decltpe), term(x$$macro$$3.body))
      |  def """.stripMargin) + new String(s"""|liftDeclGivenAnonymous(x$$macro$$3: scala.meta.Decl.GivenAnonymous) = treeByMode('{_root_.scala.meta.Decl.GivenAnonymous.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.paramClauseGroups), term(x$$macro$$3.decltpe))
      |  def liftDeclGiven(x$$macro$$3: scala.meta.Decl.Given) = treeByMode('{_root_.scala.meta.Decl.Given.After_4_12_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.paramClauseGroups), term(x$$macro$$3.decltpe))
      |  def liftDefnExtensionGroup(x$$macro$$3: scala.meta.Defn.ExtensionGroup) = treeByMode('{_root_.scala.meta.Defn.ExtensionGroup.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.paramClauseGroup), term(x$$macro$$3.body))
      |  def liftImport(x$$macro$$3: scala.meta.Import) = treeByMode('{_root_.scala.meta.Import.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.importers))
      |  def liftExport(x$$macro$$3: scala.meta.Export) = treeByMode('{_root_.scala.meta.Export.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.importers))
      |  def liftTermArgClause(x$$macro$$3: scala.meta.Term.ArgClause) = treeByMode('{_root_.scala.meta.Term.ArgClause.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.values), term(x$$macro$$3.mod))
      |  def liftTypeArgClause(x$$macro$$3: scala.meta.Type.ArgClause) = treeByMode('{_root_.scala.meta.Type.ArgClause.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.values))
      |  def liftPatArgClause(x$$macro$$3: scala.meta.Pat.ArgClause) = treeByMode('{_root_.scala.meta.Pat.ArgClause.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.values))
      |  def liftTermParamClause(x$$macro$$3: scala.meta.Term.ParamClause) = treeByMode('{_root_.scala.meta.Term.ParamClause.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.values), term(x$$macro$$3.mod))
      |  def liftTypeParamClause(x$$macro$$3: scala.meta.Type.ParamClause) = treeByMode('{_root_.scala.meta.Type.ParamClause.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.values))
      |  def liftCase(x$$macro$$3: scala.meta.Case) = treeByMode('{_root_.scala.meta.Case.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.pat), term(x$$macro$$3.cond), term(x$$macro$$3.body))
      |  def liftEnumeratorGenerator(x$$macro$$3: scala.meta.Enumerator.Generator) = treeByMode('{_root_.scala.meta.Enumerator.Generator.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.pat), term(x$$macro$$3.rhs))
      |  def liftEnumeratorCaseGenerator(x$$macro$$3: scala.meta.Enumerator.CaseGenerator) = treeByMode('{_root_.scala.meta.Enumerator.CaseGenerator.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.pat), term(x$$macro$$3.rhs))
      |  def liftEnumeratorVal(x$$macro$$3: scala.meta.Enumerator.Val) = treeByMode('{_root_.scala.meta.Enumerator.Val.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.pat), term(x$$macro$$3.rhs))
      |  def liftTypeCase(x$$macro$$3: scala.meta.TypeCase) = treeByMode('{_root_.scala.meta.TypeCase.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.pat), term(x$$macro$$3.body))
      |  def liftEnumeratorGuard(x$$macro$$3: scala.meta.Enumerator.Guard) = treeByMode('{_root_.scala.meta.Enumerator.Guard.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.cond))
      |  def liftTypeParam(x$$macro$$3: scala.meta.Type.Param) = treeByMode('{_root_.scala.meta.Type.Param.After_4_12_3}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.tparamClause), term(x$$macro$$3.bounds))
      |  def liftTermParam(x$$macro$$3: scala.meta.Term.Param) = treeByMode('{_root_.scala.meta.Term.Param.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.decltpe), term(x$$macro$$3.default))
      |  def liftSelf(x$$macro$$3: scala.meta.Self) = treeByMode('{_root_.scala.meta.Self.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.name), term(x$$macro$$3.decltpe))
      |  def liftCtorPrimary(x$$macro$$3: scala.meta.Ctor.Primary) = treeByMode('{_root_.scala.meta.Ctor.Primary.After_4_6_0}, term(x$$macro$$3.origin), term(x$$macro$$3.mods), term(x$$macro$$3.name), term(x$$macro$$3.paramClauses))
      |  def liftModAnnot(x$$macro$$3: scala.meta.Mod.Annot) = treeByMode('{_root_.scala.meta.Mod.Annot.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.init))
      |  def liftModPrivate(x$$macro$$3: scala.meta.Mod.Private) = treeByMode('{_root_.scala.meta.Mod.Private.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.within))
      |  def liftModProtected(x$$macro$$3: scala.meta.Mod.Protected) = treeByMode('{_root_.scala.meta.Mod.Protected.Initial}, term(x$$macro$$3.origin), term(x$$macro$$3.within))
      |  def liftModFinal(x$$macro$$3: scala.meta.Mod.Final) = treeByMode('{_root_.scala.meta.Mod.Final.Initial}, term(x$$macro$$3.origin))
      |  def liftModSealed(x$$macro$$3: scala.meta.Mod.Sealed) = treeByMode('{_root_.scala.meta.Mod.Sealed.Initial}, term(x$$macro$$3.origin))
      |  def liftModOpen(x$$macro$$3: scala.meta.Mod.Open) = treeByMode('{_root_.scala.meta.Mod.Open.Initial}, term(x$$macro$$3.origin))
      |  def liftModSuper(x$$macro$$3: scala.meta.Mod.Super) = treeByMode('{_root_.scala.meta.Mod.Super.Initial}, term(x$$macro$$3.origin))
      |  def liftModOverride(x$$macro$$3: scala.meta.Mod.Override) = treeByMode('{_root_.scala.meta.Mod.Override.Initial}, term(x$$macro$$3.origin))""".stripMargin) + new String(s"""|  def liftModCase(x$$macro$$3: scala.meta.Mod.Case) = treeByMode('{_root_.scala.meta.Mod.Case.Initial}, term(x$$macro$$3.origin))
      |  def liftModAbstract(x$$macro$$3: scala.meta.Mod.Abstract) = treeByMode('{_root_.scala.meta.Mod.Abstract.Initial}, term(x$$macro$$3.origin))
      |  def liftModLazy(x$$macro$$3: scala.meta.Mod.Lazy) = treeByMode('{_root_.scala.meta.Mod.Lazy.Initial}, term(x$$macro$$3.origin))
      |  def liftModValParam(x$$macro$$3: scala.meta.Mod.ValParam) = treeByMode('{_root_.scala.meta.Mod.ValParam.Initial}, term(x$$macro$$3.origin))
      |  def liftModVarParam(x$$macro$$3: scala.meta.Mod.VarParam) = treeByMode('{_root_.scala.meta.Mod.VarParam.Initial}, term(x$$macro$$3.origin))
      |  def liftModInfix(x$$macro$$3: scala.meta.Mod.Infix) = treeByMode('{_root_.scala.meta.Mod.Infix.Initial}, term(x$$macro$$3.origin))
      |  def liftModInline(x$$macro$$3: scala.meta.Mod.Inline) = treeByMode('{_root_.scala.meta.Mod.Inline.Initial}, term(x$$macro$$3.origin))
      |  def liftModOpaque(x$$macro$$3: scala.meta.Mod.Opaque) = treeByMode('{_root_.scala.meta.Mod.Opaque.Initial}, term(x$$macro$$3.origin))
      |  def liftModTransparent(x$$macro$$3: scala.meta.Mod.Transparent) = treeByMode('{_root_.scala.meta.Mod.Transparent.Initial}, term(x$$macro$$3.origin))
      |  def liftModErased(x$$macro$$3: scala.meta.Mod.Erased) = treeByMode('{_root_.scala.meta.Mod.Erased.Initial}, term(x$$macro$$3.origin))
      |  def liftModTracked(x$$macro$$3: scala.meta.Mod.Tracked) = treeByMode('{_root_.scala.meta.Mod.Tracked.Initial}, term(x$$macro$$3.origin))
      |  def liftModInto(x$$macro$$3: scala.meta.Mod.Into) = treeByMode('{_root_.scala.meta.Mod.Into.Initial}, term(x$$macro$$3.origin))
      |  def liftModImplicit(x$$macro$$3: scala.meta.Mod.Implicit) = treeByMode('{_root_.scala.meta.Mod.Implicit.Initial}, term(x$$macro$$3.origin))
      |  def liftModUsing(x$$macro$$3: scala.meta.Mod.Using) = treeByMode('{_root_.scala.meta.Mod.Using.Initial}, term(x$$macro$$3.origin))
      |  def liftModCovariant(x$$macro$$3: scala.meta.Mod.Covariant) = treeByMode('{_root_.scala.meta.Mod.Covariant.Initial}, term(x$$macro$$3.origin))
      |  def liftModContravariant(x$$macro$$3: scala.meta.Mod.Contravariant) = treeByMode('{_root_.scala.meta.Mod.Contravariant.Initial}, term(x$$macro$$3.origin))
      |
      |  def liftableSubTree0[T <: MetaTree](y: T)(using Quotes): Tree = {
      |    y match {
      |      case y : scala.meta.internal.trees.Quasi => liftQuasi(y)
      |      case y : scala.meta.Type.FuncParamClause => liftTypeFuncParamClause(y)
      |      case y : scala.meta.Type.Bounds => liftTypeBounds(y)
      |      case y : scala.meta.Member.ParamClauseGroup => liftMemberParamClauseGroup(y)
      |      case y : scala.meta.Template => liftTemplate(y)
      |      case y : scala.meta.Importer => liftImporter(y)
      |      case y : scala.meta.Source => liftSource(y)
      |      case y : scala.meta.MultiSource => liftMultiSource(y)
      |      case y : scala.meta.Stat.Block => liftStatBlock(y)
      |      case y : scala.meta.Term.EnumeratorsBlock => liftTermEnumeratorsBlock(y)
      |      case y : scala.meta.Term.Block => liftTermBlock(y)
      |      case y : scala.meta.Type.Block => liftTypeBlock(y)
      |      case y : scala.meta.Pkg.Body => liftPkgBody(y)
      |      case y : scala.meta.Ctor.Block => liftCtorBlock(y)
      |      case y : scala.meta.Template.Body => liftTemplateBody(y)
      |      case y : scala.meta.Term.CasesBlock => liftTermCasesBlock(y)
      |      case y : scala.meta.Term.PartialFunction => liftTermPartialFunction(y)
      |      case y : scala.meta.Type.CasesBlock => liftTypeCasesBlock(y)
      |      case y : scala.meta.Init => liftInit(y)
      |      case y : scala.meta.Name.Anonymous => liftNameAnonymous(y)
      |      case y : scala.meta.Name.This => liftNameThis(y)
      |      case y : scala.meta.Name.Indeterminate => liftNameIndeterminate(y)
      |      case y : scala.meta.Name.Placeholder => liftNamePlaceholder(y)
      |      case y : scala.meta.Term.Name => liftTermName(y)
      |      case y : scala.meta.Term.CapSetName => liftTermCapSetName(y)
      |      case y : scala.meta.Term.Anonymous => liftTermAnonymous(y)
      |      case y : scala.meta.Type.Name => liftTypeName(y)
      |      case y : scala.meta.Type.CapSetName => liftTypeCapSetName(y)
      |      case y : scala.meta.Term.This => liftTermThis(y)
      |      case y : scala.meta.Term.Super => liftTermSuper(y)
      |      case y : scala.meta.Term.ApplyUnary => liftTermApplyUnary(y)
      |      case y : scala.meta.Term.Select => liftTermSelect(y)
      |      case y : scala.meta.Term.SelectPostfix => liftTermSelectPostfix(y)
      |      case y : scala.meta.Type.Select => liftTypeSelect(y)
      |      case y : scala.meta.Type.Project => liftTypeProject(y)
      |      case y : scala.meta.Type.Singleton => liftTypeSingleton(y)
      |      case y : scala.meta.Type.BoundsAlias => liftTypeBoundsAlias(y)
      |      case y : scala.meta.Importee.Wildcard => liftImporteeWildcard(y)
      |      case y : scala.meta.Importee.Given => liftImporteeGiven(y)
      |      case y : scala.meta.Importee.GivenAll => liftImporteeGivenAll(y)
      |      case y : scala.meta.Importee.Name => liftImporteeName(y)
      |      case y : scala.meta.Importee.Rename => liftImporteeRename(y)
      |      case y : scala.meta.Importee.Unimport => liftImporteeUnimport(y)
      |      case y : scala.meta.Type.An""".stripMargin) + new String(s"""|onymousName => liftTypeAnonymousName(y)
      |      case y : scala.meta.Type.Apply => liftTypeApply(y)
      |      case y : scala.meta.Type.ApplyInfix => liftTypeApplyInfix(y)
      |      case y : scala.meta.Type.PolyFunction => liftTypePolyFunction(y)
      |      case y : scala.meta.Type.ImplicitFunction => liftTypeImplicitFunction(y)
      |      case y : scala.meta.Type.Tuple => liftTypeTuple(y)
      |      case y : scala.meta.Type.With => liftTypeWith(y)
      |      case y : scala.meta.Type.And => liftTypeAnd(y)
      |      case y : scala.meta.Type.Or => liftTypeOr(y)
      |      case y : scala.meta.Type.Refine => liftTypeRefine(y)
      |      case y : scala.meta.Type.Existential => liftTypeExistential(y)
      |      case y : scala.meta.Type.Annotate => liftTypeAnnotate(y)
      |      case y : scala.meta.Type.Lambda => liftTypeLambda(y)
      |      case y : scala.meta.Type.AnonymousLambda => liftTypeAnonymousLambda(y)
      |      case y : scala.meta.Type.Macro => liftTypeMacro(y)
      |      case y : scala.meta.Type.Method => liftTypeMethod(y)
      |      case y : scala.meta.Type.PatWildcard => liftTypePatWildcard(y)
      |      case y : scala.meta.Type.Repeated => liftTypeRepeated(y)
      |      case y : scala.meta.Type.Var => liftTypeVar(y)
      |      case y : scala.meta.Type.Assign => liftTypeAssign(y)
      |      case y : scala.meta.Type.Match => liftTypeMatch(y)
      |      case y : scala.meta.Type.Capturing => liftTypeCapturing(y)
      |      case y : scala.meta.Lit.Null => liftLitNull(y)
      |      case y : scala.meta.Lit.Int => liftLitInt(y)
      |      case y : scala.meta.Lit.Double => liftLitDouble(y)
      |      case y : scala.meta.Lit.Float => liftLitFloat(y)
      |      case y : scala.meta.Lit.Byte => liftLitByte(y)
      |      case y : scala.meta.Lit.Short => liftLitShort(y)
      |      case y : scala.meta.Lit.Char => liftLitChar(y)
      |      case y : scala.meta.Lit.Long => liftLitLong(y)
      |      case y : scala.meta.Lit.Boolean => liftLitBoolean(y)
      |      case y : scala.meta.Lit.Unit => liftLitUnit(y)
      |      case y : scala.meta.Lit.String => liftLitString(y)
      |      case y : scala.meta.Lit.Symbol => liftLitSymbol(y)
      |      case y : scala.meta.Type.PureFunction => liftTypePureFunction(y)
      |      case y : scala.meta.Type.PureContextFunction => liftTypePureContextFunction(y)
      |      case y : scala.meta.Type.Function => liftTypeFunction(y)
      |      case y : scala.meta.Type.ContextFunction => liftTypeContextFunction(y)
      |      case y : scala.meta.Type.ByName => liftTypeByName(y)
      |      case y : scala.meta.Type.PureByName => liftTypePureByName(y)
      |      case y : scala.meta.Type.Placeholder.Impl => liftTypePlaceholderImpl(y)
      |      case y : scala.meta.Type.Wildcard => liftTypeWildcard(y)
      |      case y : scala.meta.Type.AnonymousParam => liftTypeAnonymousParam(y)
      |      case y : scala.meta.Type.TypedParam => liftTypeTypedParam(y)
      |      case y : scala.meta.Type.FunctionArg => liftTypeFunctionArg(y)
      |      case y : scala.meta.Pat.Var => liftPatVar(y)
      |      case y : scala.meta.Pat.Wildcard => liftPatWildcard(y)
      |      case y : scala.meta.Pat.SeqWildcard => liftPatSeqWildcard(y)
      |      case y : scala.meta.Pat.Bind => liftPatBind(y)
      |      case y : scala.meta.Pat.Alternative => liftPatAlternative(y)
      |      case y : scala.meta.Pat.Tuple => liftPatTuple(y)
      |      case y : scala.meta.Pat.Repeated => liftPatRepeated(y)
      |      case y : scala.meta.Pat.Extract => liftPatExtract(y)
      |      case y : scala.meta.Pat.ExtractInfix => liftPatExtractInfix(y)
      |      case y : scala.meta.Pat.Interpolate => liftPatInterpolate(y)
      |      case y : scala.meta.Pat.Xml => liftPatXml(y)
      |      case y : scala.meta.Pat.Typed => liftPatTyped(y)
      |      case y : scala.meta.Pat.Assign => liftPatAssign(y)
      |      case y : scala.meta.Pat.Macro => liftPatMacro(y)
      |      case y : scala.meta.Pat.Given => liftPatGiven(y)
      |      case y : scala.meta.Pkg => liftPkg(y)
      |      case y : scala.meta.Pkg.Object => liftPkgObject(y)
      |      case y : scala.meta.Ctor.Secondary => liftCtorSecondary(y)
      |      case y : scala.meta.Term.Interpolate => liftTermInterpolate(y)
      |      case y : scala.meta.Term.Xml => liftTermXml(y)
      |      case y : scala.meta.Term.Apply => liftTermApply(y)
      |      case y : scala.meta.Term.ApplyUsing => liftTermApplyUsing(y)
      |      case y : scala.meta.Term.ApplyType => liftTermApplyType(y)
      |      case y : scala.meta.Term.ApplyInfix => liftTermApplyInfix(y)
      |      case y : scala.meta.Term.Assign => liftTermAssign(y)
      |      case y : scala.meta.Term.Return => liftTermReturn(y)
      |      case y : scala.meta.Term.Throw => liftTermThrow(y)
      |      case y : scala.meta.Term.Ascribe => liftTermAscribe(y)
      |      case y : scala.meta.Term.Annotate => liftTermAnnotate(y)
      |      case y : scala.meta.Term.Tuple => liftTermTuple(y)
      |      case y : scala.meta.Term.EndMarker => liftTermEndMarker(y)
      |      case y : scala.meta.Term.If => liftTermIf(y)
      |      case y : scala.meta.Term.QuotedMacroExpr => liftTermQuotedMacroExpr(y)
      |      case y : scala.meta.Term.QuotedMacroType => liftTermQuotedMacroType(y)
      |      case y : scala.meta.Term.SplicedMacroExpr => liftTermSplicedMacroExpr(y)
      |      case y : scala.meta.Term.SplicedMacroPat => liftTermSplicedMacroPat(y)
      |      case y : scala.m""".stripMargin) + new String(s"""|eta.Term.AnonymousFunction => liftTermAnonymousFunction(y)
      |      case y : scala.meta.Term.PolyFunction => liftTermPolyFunction(y)
      |      case y : scala.meta.Term.While => liftTermWhile(y)
      |      case y : scala.meta.Term.Do => liftTermDo(y)
      |      case y : scala.meta.Term.New => liftTermNew(y)
      |      case y : scala.meta.Term.NewAnonymous => liftTermNewAnonymous(y)
      |      case y : scala.meta.Term.Placeholder => liftTermPlaceholder(y)
      |      case y : scala.meta.Term.Eta => liftTermEta(y)
      |      case y : scala.meta.Term.Repeated => liftTermRepeated(y)
      |      case y : scala.meta.Term.Match => liftTermMatch(y)
      |      case y : scala.meta.Term.SelectMatch => liftTermSelectMatch(y)
      |      case y : scala.meta.Term.Try => liftTermTry(y)
      |      case y : scala.meta.Term.TryWithHandler => liftTermTryWithHandler(y)
      |      case y : scala.meta.Term.ContextFunction => liftTermContextFunction(y)
      |      case y : scala.meta.Term.Function => liftTermFunction(y)
      |      case y : scala.meta.Term.For => liftTermFor(y)
      |      case y : scala.meta.Term.ForYield => liftTermForYield(y)
      |      case y : scala.meta.Decl.Val => liftDeclVal(y)
      |      case y : scala.meta.Decl.Var => liftDeclVar(y)
      |      case y : scala.meta.Decl.Def => liftDeclDef(y)
      |      case y : scala.meta.Defn.Val => liftDefnVal(y)
      |      case y : scala.meta.Defn.Var => liftDefnVar(y)
      |      case y : scala.meta.Defn.Enum => liftDefnEnum(y)
      |      case y : scala.meta.Defn.EnumCase => liftDefnEnumCase(y)
      |      case y : scala.meta.Defn.RepeatedEnumCase => liftDefnRepeatedEnumCase(y)
      |      case y : scala.meta.Defn.Def => liftDefnDef(y)
      |      case y : scala.meta.Defn.Macro => liftDefnMacro(y)
      |      case y : scala.meta.Defn.Class => liftDefnClass(y)
      |      case y : scala.meta.Defn.Trait => liftDefnTrait(y)
      |      case y : scala.meta.Defn.Object => liftDefnObject(y)
      |      case y : scala.meta.Decl.Type => liftDeclType(y)
      |      case y : scala.meta.Defn.Type => liftDefnType(y)
      |      case y : scala.meta.Defn.Given => liftDefnGiven(y)
      |      case y : scala.meta.Defn.GivenAlias => liftDefnGivenAlias(y)
      |      case y : scala.meta.Decl.GivenAnonymous => liftDeclGivenAnonymous(y)
      |      case y : scala.meta.Decl.Given => liftDeclGiven(y)
      |      case y : scala.meta.Defn.ExtensionGroup => liftDefnExtensionGroup(y)
      |      case y : scala.meta.Import => liftImport(y)
      |      case y : scala.meta.Export => liftExport(y)
      |      case y : scala.meta.Term.ArgClause => liftTermArgClause(y)
      |      case y : scala.meta.Type.ArgClause => liftTypeArgClause(y)
      |      case y : scala.meta.Pat.ArgClause => liftPatArgClause(y)
      |      case y : scala.meta.Term.ParamClause => liftTermParamClause(y)
      |      case y : scala.meta.Type.ParamClause => liftTypeParamClause(y)
      |      case y : scala.meta.Case => liftCase(y)
      |      case y : scala.meta.Enumerator.Generator => liftEnumeratorGenerator(y)
      |      case y : scala.meta.Enumerator.CaseGenerator => liftEnumeratorCaseGenerator(y)
      |      case y : scala.meta.Enumerator.Val => liftEnumeratorVal(y)
      |      case y : scala.meta.TypeCase => liftTypeCase(y)
      |      case y : scala.meta.Enumerator.Guard => liftEnumeratorGuard(y)
      |      case y : scala.meta.Type.Param => liftTypeParam(y)
      |      case y : scala.meta.Term.Param => liftTermParam(y)
      |      case y : scala.meta.Self => liftSelf(y)
      |      case y : scala.meta.Ctor.Primary => liftCtorPrimary(y)
      |      case y : scala.meta.Mod.Annot => liftModAnnot(y)
      |      case y : scala.meta.Mod.Private => liftModPrivate(y)
      |      case y : scala.meta.Mod.Protected => liftModProtected(y)
      |      case y : scala.meta.Mod.Final => liftModFinal(y)
      |      case y : scala.meta.Mod.Sealed => liftModSealed(y)
      |      case y : scala.meta.Mod.Open => liftModOpen(y)
      |      case y : scala.meta.Mod.Super => liftModSuper(y)
      |      case y : scala.meta.Mod.Override => liftModOverride(y)
      |      case y : scala.meta.Mod.Case => liftModCase(y)
      |      case y : scala.meta.Mod.Abstract => liftModAbstract(y)
      |      case y : scala.meta.Mod.Lazy => liftModLazy(y)
      |      case y : scala.meta.Mod.ValParam => liftModValParam(y)
      |      case y : scala.meta.Mod.VarParam => liftModVarParam(y)
      |      case y : scala.meta.Mod.Infix => liftModInfix(y)
      |      case y : scala.meta.Mod.Inline => liftModInline(y)
      |      case y : scala.meta.Mod.Opaque => liftModOpaque(y)
      |      case y : scala.meta.Mod.Transparent => liftModTransparent(y)
      |      case y : scala.meta.Mod.Erased => liftModErased(y)
      |      case y : scala.meta.Mod.Tracked => liftModTracked(y)
      |      case y : scala.meta.Mod.Into => liftModInto(y)
      |      case y : scala.meta.Mod.Implicit => liftModImplicit(y)
      |      case y : scala.meta.Mod.Using => liftModUsing(y)
      |      case y : scala.meta.Mod.Covariant => liftModCovariant(y)
      |      case y : scala.meta.Mod.Contravariant => liftModContravariant(y)
      |      case _ => sys.error("none of leafs matched " + (y.getClass.getSimpleName))
      |    }
      |  }
      |}""".stripMargin)
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}