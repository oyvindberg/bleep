
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForSbtRoutesCompiler extends BleepCodegenScript("GenerateForSbtRoutesCompiler") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set(s"""|sbt-routes-compiler""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|play/routes/compiler/inject/twirl/forwardsRouter.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package play.routes.compiler.inject.twirl
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |/*1.2*/import play.routes.compiler._
      |/*2.2*/import play.routes.compiler.templates._
      |/*3.2*/import InjectedRoutesGenerator.Dependency
      |
      |object forwardsRouter extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template6[RoutesSourceInfo,Option[String],Seq[String],Seq[Dependency[Rule]],Seq[Dependency[Rule]],Seq[Dependency[Include]],play.routes.compiler.ScalaFormat.Appendable] {
      |
      |  /**/
      |  def apply/*5.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], imports: Seq[String],
      |  deps: Seq[Dependency[Rule]], rules: Seq[Dependency[Rule]], includes: Seq[Dependency[Include]]):play.routes.compiler.ScalaFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*7.1*/(${"\"" * 3}// @GENERATOR:play-routes-compiler
      |// @SOURCE:${"\"" * 3}),_display_(/*8.14*/sourceInfo/*8.24*/.source),format.raw/*8.31*/(${"\"" * 3}
      |
      |${"\"" * 3}),_display_(/*10.2*/for(p <- pkg) yield /*10.15*/ {_display_(Seq[Any](format.raw/*10.17*/(${"\"" * 3}package ${"\"" * 3}),_display_(/*10.26*/p)))}),format.raw/*10.28*/(${"\"" * 3}
      |
      |${"\"" * 3}),format.raw/*12.1*/(${"\"" * 3}import play.core.routing._
      |import play.core.routing.HandlerInvokerFactory._
      |
      |import play.api.mvc._
      |${"\"" * 3}),_display_(/*16.2*/for(i <- imports) yield /*16.19*/ {_display_(Seq[Any](format.raw/*16.21*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*17.1*/(${"\"" * 3}import ${"\"" * 3}),_display_(if(!i.startsWith("_root_."))/*17.37*/{_display_(Seq[Any](format.raw/*17.38*/(${"\"" * 3}_root_.${"\"" * 3})))} else {null} ),_display_(/*17.47*/i)))}),format.raw/*17.49*/(${"\"" * 3}
      |
      |${"\"" * 3}),format.raw/*19.1*/(${"\"" * 3}class Routes(
      |  override val errorHandler: play.api.http.HttpErrorHandler, ${"\"" * 3}),_display_(/*20.63*/for(dep <- deps) yield /*20.79*/{_display_(Seq[Any](format.raw/*20.80*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*21.4*/markLines(dep.rule)),format.raw/*21.23*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*22.4*/dep/*22.7*/.ident),format.raw/*22.13*/(${"\"" * 3}: ${"\"" * 3}),_display_(/*22.16*/dep/*22.19*/.clazz),format.raw/*22.25*/(${"\"" * 3},${"\"" * 3})))}),format.raw/*22.27*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*23.3*/(${"\"" * 3}val prefix: String
      |) extends GeneratedRouter ${"\"" * 3}),_display_(/*24.28*/ob),format.raw/*24.30*/(${"\"" * 3}
      |
      |  ${"\"" * 3}),format.raw/*26.3*/(${"\"" * 3}@jakarta.inject.Inject()
      |  def this(errorHandler: play.api.http.HttpErrorHandler${"\"" * 3}),_display_(/*27.57*/for(dep <- deps) yield /*27.73*/ {_display_(Seq[Any](format.raw/*27.75*/(${"\"" * 3},
      |    ${"\"" * 3}),_display_(/*28.6*/markLines(dep.rule)),format.raw/*28.25*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*29.6*/dep/*29.9*/.ident),format.raw/*29.15*/(${"\"" * 3}: ${"\"" * 3}),_display_(/*29.18*/dep/*29.21*/.clazz)))}),format.raw/*29.28*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*30.3*/(${"\"" * 3}) = this(errorHandler, ${"\"" * 3}),_display_(/*30.27*/for(dep <- deps) yield /*30.43*/{_display_(Seq[Any](_display_(/*30.45*/dep/*30.48*/.ident),format.raw/*30.54*/(${"\"" * 3}, ${"\"" * 3})))}),format.raw/*30.57*/(${"\"" * 3}"/")
      |
      |  def withPrefix(addPrefix: String): Routes = ${"\"" * 3}),_display_(/*32.48*/ob),format.raw/*32.50*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*33.5*/(${"\"" * 3}val prefix = play.api.routing.Router.concatPrefix(addPrefix, this.prefix)
      |    ${"\"" * 3}),_display_(/*34.6*/(pkg.getOrElse("_routes_"))),format.raw/*34.33*/(${"\"" * 3}.RoutesPrefix.setPrefix(prefix)
      |    new Routes(errorHandler, ${"\"" * 3}),_display_(/*35.31*/for(dep <- deps) yield /*35.47*/{_display_(Seq[Any](_display_(/*35.49*/dep/*35.52*/.ident),format.raw/*35.58*/(${"\"" * 3}, ${"\"" * 3})))}),format.raw/*35.61*/(${"\"" * 3}prefix)
      |  ${"\"" * 3}),_display_(/*36.4*/cb),format.raw/*36.6*/(${"\"" * 3}
      |
      |  ${"\"" * 3}),format.raw/*38.3*/(${"\"" * 3}private val defaultPrefix: String = ${"\"" * 3}),_display_(/*38.40*/ob),format.raw/*38.42*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*39.5*/(${"\"" * 3}if (this.prefix.endsWith("/")) "" else "/"
      |  ${"\"" * 3}),_display_(/*40.4*/cb),format.raw/*40.6*/(${"\"" * 3}
      |
      |  ${"\"" * 3}),format.raw/*42.3*/(${"\"" * 3}def documentation = List(${"\"" * 3}),_display_(/*42.29*/for((dep, index) <- rules.zipWithIndex) yield /*42.68*/ {_display_(Seq[Any](format.raw/*42.70*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*43.6*/dep/*43.9*/.rule/*43.14*/ match/*43.20*/ {/*44.7*/case Route(verb, path, call, _, _) if path.parts.isEmpty =>/*44.66*/ {_display_(Seq[Any](format.raw/*44.68*/(${"\"" * 3}(${"\"" * 3}),_display_(/*44.70*/tq),_display_(/*44.73*/verb),_display_(/*44.78*/tq),format.raw/*44.80*/(${"\"" * 3}, this.prefix, ${"\"" * 3}),_display_(/*44.96*/tq),_display_(/*44.99*/call),_display_(/*44.104*/tq),format.raw/*44.106*/(${"\"" * 3})${"\"" * 3})))}/*45.7*/case Route(verb, path, call, _, _) =>/*45.44*/ {_display_(Seq[Any](format.raw/*45.46*/(${"\"" * 3}(${"\"" * 3}),_display_(/*45.48*/tq),_display_(/*45.51*/verb),_display_(/*45.56*/tq),format.raw/*45.58*/(${"\"" * 3}, this.prefix + (if(this.prefix.endsWith("/")) "" else "/") + ${"\"" * 3}),_display_(/*45.121*/encodeStringConstant(path.toString)),format.raw/*45.156*/(${"\"" * 3}, ${"\"" * 3}),_display_(/*45.159*/tq),_display_(/*45.162*/call),_display_(/*45.167*/tq),format.raw/*45.169*/(${"\"" * 3})${"\"" * 3})))}/*46.7*/case include: Include =>/*46.31*/ {_display_(Seq[Any](format.raw/*46.33*/(${"\"" * 3}prefixed_${"\"" * 3}),_display_(/*46.43*/(dep.ident)),format.raw/*46.54*/(${"\"" * 3}_${"\"" * 3}),_display_(/*46.56*/(index)),format.raw/*46.63*/(${"\"" * 3}.router.documentation${"\"" * 3})))}}),format.raw/*47.4*/(${"\"" * 3},${"\"" * 3})))}),format.raw/*47.6*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*48.5*/(${"\"" * 3}Nil
      |  ).foldLeft(Seq.empty[(String, String, String)]) ${"\"" * 3}),format.raw/*49.51*/(${"\"" * 3}{${"\"" * 3}),format.raw/*49.52*/(${"\"" * 3} ${"\"" * 3}),format.raw/*49.53*/(${"\"" * 3}(s,e) => e.asInstanceOf[Any] match ${"\"" * 3}),format.raw/*49.88*/(${"\"" * 3}{${"\"" * 3}),format.raw/*49.89*/(${"\"" * 3}
      |    case r @ (_,_,_) => s :+ r.asInstanceOf[(String, String, String)]
      |    case l => s ++ l.asInstanceOf[List[(String, String, String)]]
      |  ${"\"" * 3}),format.raw/*52.3*/(${"\"" * 3}}${"\"" * 3}),format.raw/*52.4*/(${"\"" * 3}}${"\"" * 3}),format.raw/*52.5*/(${"\"" * 3}
      |
      |${"\"" * 3}),_display_(/*54.2*/for((dep, index) <- rules.zipWithIndex) yield /*54.41*/{_display_(_display_(/*54.43*/dep/*54.46*/.rule/*54.51*/ match/*54.57*/ {/*55.1*/case route @ Route(verb, path, call, comments, modifiers) =>/*55.61*/ {_display_(Seq[Any](format.raw/*55.63*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*56.4*/markLines(route)),format.raw/*56.20*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*57.3*/(${"\"" * 3}private lazy val ${"\"" * 3}),_display_(/*57.21*/routeIdentifier(route, index)),format.raw/*57.50*/(${"\"" * 3} ${"\"" * 3}),format.raw/*57.51*/(${"\"" * 3}= Route(${"\"" * 3}"),_display_(/*57.61*/verb/*57.65*/.value),format.raw/*57.71*/(${"\"" * 3}",
      |    PathPattern(List(StaticPart(this.prefix)${"\"" * 3}),_display_(if(path.parts.nonEmpty)/*58.69*/ {_display_(Seq[Any](format.raw/*58.71*/(${"\"" * 3}, StaticPart(this.defaultPrefix), ${"\"" * 3})))} else {null} ),_display_(/*58.107*/path/*58.111*/.parts.map(_.toString).mkString(", ")),format.raw/*58.148*/(${"\"" * 3}))
      |  )
      |  private lazy val ${"\"" * 3}),_display_(/*60.21*/invokerIdentifier(route, index)),format.raw/*60.52*/(${"\"" * 3} ${"\"" * 3}),format.raw/*60.53*/(${"\"" * 3}= createInvoker(
      |    ${"\"" * 3}),_display_(if(route.call.passJavaRequest)/*61.36*/{_display_(Seq[Any](format.raw/*61.37*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*62.5*/(${"\"" * 3}(req:play.mvc.Http.Request) =>
      |      ${"\"" * 3})))} else {null} ),_display_(/*63.9*/injectedControllerMethodCall(route, dep.ident, p => s"fakeValue[$${p.typeNameReal}]")),format.raw/*63.93*/(${"\"" * 3},
      |    play.api.routing.HandlerDef(this.getClass.getClassLoader,
      |      ${"\"" * 3}"),_display_(/*65.9*/for(p <- pkg) yield /*65.22*/ {_display_(_display_(/*65.25*/p))}),format.raw/*65.27*/(${"\"" * 3}",
      |      ${"\"" * 3}"),_display_(/*66.9*/{call.packageName.map(_ + ".").getOrElse("")}),_display_(/*66.55*/call/*66.59*/.controller),format.raw/*66.70*/(${"\"" * 3}",
      |      ${"\"" * 3}"),_display_(/*67.9*/call/*67.13*/.method),format.raw/*67.20*/(${"\"" * 3}",
      |      ${"\"" * 3}),_display_(/*68.8*/call/*68.12*/.parameters.filterNot(_.isEmpty).map(params => params.map("classOf[" + _.typeNameReal + "]").mkString(", ")).map("Seq(" + _ + ")").getOrElse("Nil")),format.raw/*68.159*/(${"\"" * 3},
      |      ${"\"" * 3}"),_display_(/*69.9*/verb),format.raw/*69.13*/(${"\"" * 3}",
      |      this.prefix + ${"\"" * 3}),_display_(/*70.22*/encodeStringConstant(path.toString)),format.raw/*70.57*/(${"\"" * 3},
      |      ${"\"" * 3}),_display_(/*71.8*/encodeStringConstant(comments.map(_.comment).mkString("\\n"))),format.raw/*71.68*/(${"\"" * 3},
      |      Seq(${"\"" * 3}),_display_(/*72.12*/modifiers/*72.21*/.map(_.value).map(encodeStringConstant).mkString(", ")),format.raw/*72.75*/(${"\"" * 3})
      |    )
      |  )
      |${"\"" * 3})))}/*76.1*/case include @ Include(path, router) =>/*76.40*/ {_display_(Seq[Any](format.raw/*76.42*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*77.4*/markLines(include)),format.raw/*77.22*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*78.3*/(${"\"" * 3}private val prefixed_${"\"" * 3}),_display_(/*78.25*/(dep.ident)),format.raw/*78.36*/(${"\"" * 3}_${"\"" * 3}),_display_(/*78.38*/(index)),format.raw/*78.45*/(${"\"" * 3} ${"\"" * 3}),format.raw/*78.46*/(${"\"" * 3}= Include(${"\"" * 3}),_display_(/*78.57*/(dep.ident)),format.raw/*78.68*/(${"\"" * 3}.withPrefix(this.prefix + (if (this.prefix.endsWith("/")) "" else "/") + ${"\"" * 3}"),_display_(/*78.143*/include/*78.150*/.prefix),format.raw/*78.157*/(${"\"" * 3}"))
      |${"\"" * 3})))}}))}),format.raw/*79.4*/(${"\"" * 3}
      |
      |  ${"\"" * 3}),format.raw/*81.3*/(${"\"" * 3}def routes: PartialFunction[RequestHeader, Handler] = ${"\"" * 3}),_display_(/*81.58*/ob),format.raw/*81.60*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(if(rules.isEmpty)/*82.21*/ {_display_(Seq[Any](format.raw/*82.23*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*83.5*/(${"\"" * 3}Map.empty
      |  ${"\"" * 3})))}else/*84.10*/{_display_(_display_(/*84.12*/for((dep, index) <- rules.zipWithIndex) yield /*84.51*/{_display_(_display_(/*84.53*/dep/*84.56*/.rule/*84.61*/ match/*84.67*/ {/*85.3*/case include: Include =>/*85.27*/ {_display_(Seq[Any](format.raw/*85.29*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*86.6*/markLines(include)),format.raw/*86.24*/(${"\"" * 3}
      |    case prefixed_${"\"" * 3}),_display_(/*87.20*/(dep.ident)),format.raw/*87.31*/(${"\"" * 3}_${"\"" * 3}),_display_(/*87.33*/(index)),format.raw/*87.40*/(${"\"" * 3}(handler) => handler
      |  ${"\"" * 3})))}/*89.3*/case route: Route =>/*89.23*/ {_display_(Seq[Any](format.raw/*89.25*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*90.6*/markLines(route)),format.raw/*90.22*/(${"\"" * 3}
      |    case ${"\"" * 3}),_display_(/*91.11*/(routeIdentifier(route, index))),format.raw/*91.42*/(${"\"" * 3}(params@_) =>
      |      call${"\"" * 3}),_display_(/*92.12*/(routeBinding(route))),format.raw/*92.33*/(${"\"" * 3} ${"\"" * 3}),_display_(/*92.35*/ob),format.raw/*92.37*/(${"\"" * 3} ${"\"" * 3}),_display_(/*92.39*/localNames(route)),format.raw/*92.56*/(${"\"" * 3}
      |        ${"\"" * 3}),_display_(/*93.10*/(invokerIdentifier(route, index))),format.raw/*93.43*/(${"\"" * 3}.call(${"\"" * 3}),_display_(if(route.call.passJavaRequest)/*93.80*/{_display_(Seq[Any](format.raw/*93.81*/(${"\"" * 3}
      |          ${"\"" * 3}),format.raw/*94.11*/(${"\"" * 3}req => ${"\"" * 3})))} else {null} ),_display_(/*94.20*/injectedControllerMethodCall(route, dep.ident, x => if (x.isJavaRequest) "req" else safeKeyword(x.nameClean))),format.raw/*94.129*/(${"\"" * 3})
      |      ${"\"" * 3}),_display_(/*95.8*/cb),format.raw/*95.10*/(${"\"" * 3}
      |  ${"\"" * 3})))}}))}))}),_display_(/*97.7*/cb),format.raw/*97.9*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*98.2*/cb),format.raw/*98.4*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(sourceInfo:RoutesSourceInfo,pkg:Option[String],imports:Seq[String],deps:Seq[Dependency[Rule]],rules:Seq[Dependency[Rule]],includes:Seq[Dependency[Include]]): play.routes.compiler.ScalaFormat.Appendable = apply(sourceInfo,pkg,imports,deps,rules,includes)
      |
      |  def f:((RoutesSourceInfo,Option[String],Seq[String],Seq[Dependency[Rule]],Seq[Dependency[Rule]],Seq[Dependency[Include]]) => play.routes.compiler.ScalaFormat.Appendable) = (sourceInfo,pkg,imports,deps,rules,includes) => apply(sourceInfo,pkg,imports,deps,rules,includes)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/inject/forwardsRouter.scala.twirl
      |                  HASH: 82cbbda630f8e8124b671e803aa70eada1c70f72
      |                  MATRIX: 292->1|329->32|376->73|865->117|1136->288|1209->337|1227->347|1254->354|1283->357|1312->370|1352->372|1388->381|1414->383|1443->385|1569->485|1602->502|1642->504|1670->505|1733->541|1772->542|1824->551|1850->553|1879->555|1982->631|2014->647|2053->648|2083->652|2123->671|2153->675|2164->678|2191->684|2221->687|2233->690|2260->696|2293->698|2323->701|2396->747|2419->749|2450->753|2558->835|2590->851|2630->853|2663->860|2703->879|2735->885|2746->888|2773->894|2803->897|2815->900|2846->907|2876->910|2927->934|2959->950|2998->952|3010->955|3037->961|3071->964|3151->1017|3174->1019|3206->1024|3311->1103|3359->1130|3448->1192|3480->1208|3519->1210|3531->1213|3558->1219|3592->1222|3629->1233|3651->1235|3682->1239|3746->1276|3769->1278|3801->1283|3873->1329|3895->1331|3926->1335|3979->1361|4034->1400|4074->1402|4106->1408|4117->1411|4131->1416|4146->1422|4156->1431|4224->1490|4264->1492|4293->1494|4316->1497|4341->1502|4364->1504|4407->1520|4430->1523|4456->1528|4480->1530|4500->1539|4546->1576|4586->1578|4615->1580|4638->1583|4663->1588|4686->1590|4777->1653|4834->1688|4865->1691|4889->1694|4915->1699|4939->1701|4959->1710|4992->1734|5032->1736|5069->1746|5101->1757|5130->1759|5158->1766|5211->1792|5243->1794|5275->1799|5357->1853|5386->1854|5415->1855|5478->1890|5507->1891|5673->2031|5701->2032|5729->2033|5758->2036|5813->2075|5843->2077|5855->2080|5869->2085|5884->2091|5894->2094|5963->2154|6003->2156|6033->2160|6070->2176|6100->2179|6145->2197|6195->2226|6224->2227|6261->2237|6274->2241|6301->2247|6399->2318|6439->2320|6519->2356|6533->2360|6592->2397|6646->2424|6698->2455|6727->2456|6806->2508|6845->2509|6877->2514|6958->2553|7063->2637|7161->2709|7190->2722|7221->2725|7246->2727|7283->2738|7349->2784|7362->2788|7394->2799|7431->2810|7444->2814|7472->2821|7508->2831|7521->2835|7690->2982|7726->2992|7751->2996|7802->3020|7858->3055|7893->3064|7974->3124|8014->3137|8032->3146|8107->3200|8138->3214|8186->3253|8226->3255|8256->3259|8295->3277|8325->3280|8374->3302|8406->3313|8435->3315|8463->3322|8492->3323|8530->3334|8562->3345|8665->3420|8682->3427|8711->3434|8750->3441|8781->3445|8863->3500|8886->3502|8934->3523|8974->3525|9006->3530|9042->3549|9072->3551|9127->3590|9157->3592|9169->3595|9183->3600|9198->3606|9208->3611|9241->3635|9281->3637|9313->3643|9352->3661|9399->3681|9431->3692|9460->3694|9488->3701|9530->3728|9559->3748|9599->3750|9631->3756|9668->3772|9706->3783|9758->3814|9810->3840|9852->3861|9881->3863|9904->3865|9933->3867|9971->3884|10008->3894|10062->3927|10126->3964|10165->3965|10204->3976|10256->3985|10387->4094|10422->4103|10445->4105|10486->4116|10508->4118|10536->4120|10558->4122
      |                  LINES: 10->1|11->2|12->3|17->5|23->7|24->8|24->8|24->8|26->10|26->10|26->10|26->10|26->10|28->12|32->16|32->16|32->16|33->17|33->17|33->17|33->17|33->17|35->19|36->20|36->20|36->20|37->21|37->21|38->22|38->22|38->22|38->22|38->22|38->22|38->22|39->23|40->24|40->24|42->26|43->27|43->27|43->27|44->28|44->28|45->29|45->29|45->29|45->29|45->29|45->29|46->30|46->30|46->30|46->30|46->30|46->30|46->30|48->32|48->32|49->33|50->34|50->34|51->35|51->35|51->35|51->35|51->35|51->35|52->36|52->36|54->38|54->38|54->38|55->39|56->40|56->40|58->42|58->42|58->42|58->42|59->43|59->43|59->43|59->43|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->46|59->46|59->46|59->46|59->46|59->46|59->46|59->47|59->47|60->48|61->49|61->49|61->49|61->49|61->49|64->52|64->52|64->52|66->54|66->54|66->54|66->54|66->54|66->54|66->55|66->55|66->55|67->56|67->56|68->57|68->57|68->57|68->57|68->57|68->57|68->57|69->58|69->58|69->58|69->58|69->58|71->60|71->60|71->60|72->61|72->61|73->62|74->63|74->63|76->65|76->65|76->65|76->65|77->66|77->66|77->66|77->66|78->67|78->67|78->67|79->68|79->68|79->68|80->69|80->69|81->70|81->70|82->71|82->71|83->72|83->72|83->72|86->76|86->76|86->76|87->77|87->77|88->78|88->78|88->78|88->78|88->78|88->78|88->78|88->78|88->78|88->78|88->78|89->79|91->81|91->81|91->81|92->82|92->82|93->83|94->84|94->84|94->84|94->84|94->84|94->84|94->84|94->85|94->85|94->85|95->86|95->86|96->87|96->87|96->87|96->87|97->89|97->89|97->89|98->90|98->90|99->91|99->91|100->92|100->92|100->92|100->92|100->92|100->92|101->93|101->93|101->93|101->93|102->94|102->94|102->94|103->95|103->95|104->97|104->97|105->98|105->98
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|sbt-routes-compiler""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|play/routes/compiler/static/twirl/javaWrappers.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package play.routes.compiler.static.twirl
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |/*1.2*/import play.routes.compiler._
      |/*2.2*/import play.routes.compiler.templates._
      |
      |object javaWrappers extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template5[RoutesSourceInfo,Option[String],Option[String],Seq[String],Boolean,play.routes.compiler.ScalaFormat.Appendable] {
      |
      |  /**/
      |  def apply/*3.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], packageName: Option[String], controllers: Seq[String], jsReverseRouter: Boolean):play.routes.compiler.ScalaFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*4.1*/(${"\"" * 3}// @GENERATOR:play-routes-compiler
      |// @SOURCE:${"\"" * 3}),_display_(/*5.14*/sourceInfo/*5.24*/.source),format.raw/*5.31*/(${"\"" * 3}
      |
      |${"\"" * 3}),_display_(/*7.2*/{packageName.map("package " + _ + ";").getOrElse("")}),format.raw/*7.55*/(${"\"" * 3}
      |
      |${"\"" * 3}),format.raw/*9.1*/(${"\"" * 3}import ${"\"" * 3}),_display_(/*9.9*/(pkg.getOrElse("_routes_"))),format.raw/*9.36*/(${"\"" * 3}.RoutesPrefix;
      |
      |public class routes ${"\"" * 3}),_display_(/*11.22*/ob),format.raw/*11.24*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*12.4*/for(controller <- controllers) yield /*12.34*/ {_display_(Seq[Any](format.raw/*12.36*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*13.3*/(${"\"" * 3}public static final ${"\"" * 3}),_display_(/*13.24*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*13.64*/(${"\"" * 3}Reverse${"\"" * 3}),_display_(/*13.72*/controller),format.raw/*13.82*/(${"\"" * 3} ${"\"" * 3}),_display_(/*13.84*/controller),format.raw/*13.94*/(${"\"" * 3} ${"\"" * 3}),format.raw/*13.95*/(${"\"" * 3}= new ${"\"" * 3}),_display_(/*13.102*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*13.142*/(${"\"" * 3}Reverse${"\"" * 3}),_display_(/*13.150*/(controller)),format.raw/*13.162*/(${"\"" * 3}(RoutesPrefix.byNamePrefix());${"\"" * 3})))}),format.raw/*13.193*/(${"\"" * 3}
      |${"\"" * 3}),_display_(if(jsReverseRouter)/*14.21*/ {_display_(Seq[Any](format.raw/*14.23*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*15.3*/(${"\"" * 3}public static class javascript ${"\"" * 3}),_display_(/*15.35*/ob),format.raw/*15.37*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*16.6*/for(controller <- controllers) yield /*16.36*/ {_display_(Seq[Any](format.raw/*16.38*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*17.5*/(${"\"" * 3}public static final ${"\"" * 3}),_display_(/*17.26*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*17.66*/(${"\"" * 3}javascript.Reverse${"\"" * 3}),_display_(/*17.85*/controller),format.raw/*17.95*/(${"\"" * 3} ${"\"" * 3}),_display_(/*17.97*/controller),format.raw/*17.107*/(${"\"" * 3} ${"\"" * 3}),format.raw/*17.108*/(${"\"" * 3}= new ${"\"" * 3}),_display_(/*17.115*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*17.155*/(${"\"" * 3}javascript.Reverse${"\"" * 3}),_display_(/*17.174*/(controller)),format.raw/*17.186*/(${"\"" * 3}(RoutesPrefix.byNamePrefix());${"\"" * 3})))}),format.raw/*17.217*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*18.4*/cb),format.raw/*18.6*/(${"\"" * 3}
      |${"\"" * 3})))} else {null} ),format.raw/*19.2*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*20.2*/cb),format.raw/*20.4*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(sourceInfo:RoutesSourceInfo,pkg:Option[String],packageName:Option[String],controllers:Seq[String],jsReverseRouter:Boolean): play.routes.compiler.ScalaFormat.Appendable = apply(sourceInfo,pkg,packageName,controllers,jsReverseRouter)
      |
      |  def f:((RoutesSourceInfo,Option[String],Option[String],Seq[String],Boolean) => play.routes.compiler.ScalaFormat.Appendable) = (sourceInfo,pkg,packageName,controllers,jsReverseRouter) => apply(sourceInfo,pkg,packageName,controllers,jsReverseRouter)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/static/javaWrappers.scala.twirl
      |                  HASH: 9e5bb9e12f81767633248b86e50ca93431b49c62
      |                  MATRIX: 292->1|329->32|768->73|1001->206|1074->255|1092->265|1119->272|1147->275|1220->328|1248->330|1281->338|1328->365|1392->402|1415->404|1445->408|1491->438|1531->440|1561->443|1609->464|1670->504|1705->512|1736->522|1765->524|1796->534|1825->535|1860->542|1922->582|1958->590|1992->602|2055->633|2103->654|2143->656|2173->659|2232->691|2255->693|2287->699|2333->729|2373->731|2405->736|2453->757|2514->797|2560->816|2591->826|2620->828|2652->838|2682->839|2717->846|2779->886|2826->905|2860->917|2923->948|2953->952|2975->954|3020->956|3048->958|3070->960
      |                  LINES: 10->1|11->2|16->3|21->4|22->5|22->5|22->5|24->7|24->7|26->9|26->9|26->9|28->11|28->11|29->12|29->12|29->12|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|31->14|31->14|32->15|32->15|32->15|33->16|33->16|33->16|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|35->18|35->18|36->19|37->20|37->20
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|sbt-routes-compiler""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|play/routes/compiler/static/twirl/javascriptReverseRouter.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package play.routes.compiler.static.twirl
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |/*1.2*/import play.routes.compiler._
      |/*2.2*/import play.routes.compiler.templates._
      |
      |object javascriptReverseRouter extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template7[RoutesSourceInfo,Option[String],Seq[String],Option[String],Seq[Route],Boolean,Route => Boolean,play.routes.compiler.ScalaFormat.Appendable] {
      |
      |  /**/
      |  def apply/*3.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], imports: Seq[String], packageName: Option[String], routes: Seq[Route], namespaceReverseRouter: Boolean, useInjector: Route => Boolean):play.routes.compiler.ScalaFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*4.1*/(${"\"" * 3}// @GENERATOR:play-routes-compiler
      |// @SOURCE:${"\"" * 3}),_display_(/*5.14*/sourceInfo/*5.24*/.source),format.raw/*5.31*/(${"\"" * 3}
      |
      |${"\"" * 3}),format.raw/*7.1*/(${"\"" * 3}import play.api.routing.JavaScriptReverseRoute
      |
      |${"\"" * 3}),_display_(/*9.2*/for(i <- imports) yield /*9.19*/ {_display_(Seq[Any](format.raw/*9.21*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*10.1*/(${"\"" * 3}import ${"\"" * 3}),_display_(if(!i.startsWith("_root_."))/*10.37*/{_display_(Seq[Any](format.raw/*10.38*/(${"\"" * 3}_root_.${"\"" * 3})))} else {null} ),_display_(/*10.47*/i)))}),format.raw/*10.49*/(${"\"" * 3}
      |
      |${"\"" * 3}),_display_(/*12.2*/markLines(routes: _*)),format.raw/*12.23*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*13.1*/(${"\"" * 3}package ${"\"" * 3}),_display_(/*13.10*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*13.50*/(${"\"" * 3}javascript ${"\"" * 3}),_display_(/*13.62*/ob),format.raw/*13.64*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*14.2*/for((controller, routes) <- groupRoutesByController(routes)) yield /*14.62*/ {_display_(Seq[Any](format.raw/*14.64*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*15.4*/markLines(routes: _*)),format.raw/*15.25*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*16.3*/(${"\"" * 3}class Reverse${"\"" * 3}),_display_(/*16.17*/(controller.replace(".", "_"))),format.raw/*16.47*/(${"\"" * 3}(_prefix: => String) ${"\"" * 3}),_display_(/*16.69*/ob),format.raw/*16.71*/(${"\"" * 3}
      |
      |    ${"\"" * 3}),format.raw/*18.5*/(${"\"" * 3}def _defaultPrefix: String = ${"\"" * 3}),_display_(/*18.35*/ob),format.raw/*18.37*/(${"\"" * 3}
      |      ${"\"" * 3}),format.raw/*19.7*/(${"\"" * 3}if (_prefix.endsWith("/")) "" else "/"
      |    ${"\"" * 3}),_display_(/*20.6*/cb),format.raw/*20.8*/(${"\"" * 3}
      |
      |  ${"\"" * 3}),_display_(/*22.4*/for(((method, _), routes) <- groupRoutesByMethod(routes)) yield /*22.61*/ {_display_(_display_(/*22.64*/routes/*22.70*/ match/*22.76*/ {/*23.3*/case Seq(route: Route) =>/*23.28*/ {_display_(Seq[Any](format.raw/*23.30*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*24.6*/markLines(route)),format.raw/*24.22*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*25.5*/(${"\"" * 3}def ${"\"" * 3}),_display_(/*25.10*/method),format.raw/*25.16*/(${"\"" * 3}: JavaScriptReverseRoute = JavaScriptReverseRoute(
      |      ${"\"" * 3}"),_display_(/*26.9*/{packageName.map(_ + ".").getOrElse("")}),_display_(/*26.50*/(controller)),format.raw/*26.62*/(${"\"" * 3}.${"\"" * 3}),_display_(/*26.64*/(method)),format.raw/*26.72*/(${"\"" * 3}",
      |      ${"\"" * 3}),_display_(/*27.8*/tq),format.raw/*27.10*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*28.9*/(${"\"" * 3}function(${"\"" * 3}),_display_(/*28.19*/reverseParametersJavascript(routes)/*28.54*/.map(_._1.name).mkString(",")),format.raw/*28.83*/(${"\"" * 3}) ${"\"" * 3}),_display_(/*28.86*/ob),format.raw/*28.88*/(${"\"" * 3}
      |          ${"\"" * 3}),_display_(/*29.12*/javascriptCall(route, reverseLocalNames(route, reverseParametersJavascript(routes)))),format.raw/*29.96*/(${"\"" * 3}
      |        ${"\"" * 3}),_display_(/*30.10*/cb),format.raw/*30.12*/(${"\"" * 3}
      |      ${"\"" * 3}),_display_(/*31.8*/tq),format.raw/*31.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*32.5*/(${"\"" * 3})
      |  ${"\"" * 3})))}/*34.3*/case _ =>/*34.12*/ {_display_(Seq[Any](format.raw/*34.14*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*35.6*/markLines(routes: _*)),format.raw/*35.27*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*36.5*/(${"\"" * 3}def ${"\"" * 3}),_display_(/*36.10*/method),format.raw/*36.16*/(${"\"" * 3}: JavaScriptReverseRoute = JavaScriptReverseRoute(
      |      ${"\"" * 3}"),_display_(/*37.9*/{packageName.map(_ + ".").getOrElse("")}),_display_(/*37.50*/(controller)),format.raw/*37.62*/(${"\"" * 3}.${"\"" * 3}),_display_(/*37.64*/(method)),format.raw/*37.72*/(${"\"" * 3}",
      |      ${"\"" * 3}),_display_(/*38.8*/tq),format.raw/*38.10*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*39.9*/(${"\"" * 3}function(${"\"" * 3}),_display_(/*39.19*/reverseParametersJavascript(routes)/*39.54*/.map(_._1.name).mkString(",")),format.raw/*39.83*/(${"\"" * 3}) ${"\"" * 3}),_display_(/*39.86*/ob),format.raw/*39.88*/(${"\"" * 3}
      |        ${"\"" * 3}),_display_(/*40.10*/for((route, localNames, constraints) <- javascriptCollectNonDeadRoutes(routes)) yield /*40.89*/ {_display_(Seq[Any](format.raw/*40.91*/(${"\"" * 3}
      |          ${"\"" * 3}),format.raw/*41.11*/(${"\"" * 3}if (${"\"" * 3}),_display_(/*41.16*/constraints),format.raw/*41.27*/(${"\"" * 3}) ${"\"" * 3}),_display_(/*41.30*/ob),format.raw/*41.32*/(${"\"" * 3}
      |            ${"\"" * 3}),_display_(/*42.14*/javascriptCall(route, localNames)),format.raw/*42.47*/(${"\"" * 3}
      |          ${"\"" * 3}),_display_(/*43.12*/cb),format.raw/*43.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*44.10*/(${"\"" * 3}
      |        ${"\"" * 3}),_display_(/*45.10*/cb),format.raw/*45.12*/(${"\"" * 3}
      |      ${"\"" * 3}),_display_(/*46.8*/tq),format.raw/*46.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*47.5*/(${"\"" * 3})
      |  ${"\"" * 3})))}}))}),format.raw/*48.6*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*49.4*/cb),format.raw/*49.6*/(${"\"" * 3}
      |${"\"" * 3})))}),format.raw/*50.2*/(${"\"" * 3}
      |
      |${"\"" * 3}),_display_(/*52.2*/cb),format.raw/*52.4*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(sourceInfo:RoutesSourceInfo,pkg:Option[String],imports:Seq[String],packageName:Option[String],routes:Seq[Route],namespaceReverseRouter:Boolean,useInjector:Route => Boolean): play.routes.compiler.ScalaFormat.Appendable = apply(sourceInfo,pkg,imports,packageName,routes,namespaceReverseRouter,useInjector)
      |
      |  def f:((RoutesSourceInfo,Option[String],Seq[String],Option[String],Seq[Route],Boolean,Route => Boolean) => play.routes.compiler.ScalaFormat.Appendable) = (sourceInfo,pkg,imports,packageName,routes,namespaceReverseRouter,useInjector) => apply(sourceInfo,pkg,imports,packageName,routes,namespaceReverseRouter,useInjector)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/static/javascriptReverseRouter.scala.twirl
      |                  HASH: 687898b8e353064b2be1160a0827ec95c1d0d9bd
      |                  MATRIX: 292->1|329->32|807->73|1094->260|1167->309|1185->319|1212->326|1240->328|1314->377|1346->394|1385->396|1413->397|1476->433|1515->434|1567->443|1593->445|1622->448|1664->469|1692->470|1728->479|1789->519|1828->531|1851->533|1879->535|1955->595|1995->597|2025->601|2067->622|2097->625|2138->639|2189->669|2238->691|2261->693|2294->699|2351->729|2374->731|2408->738|2478->782|2500->784|2531->789|2604->846|2635->849|2650->855|2665->861|2675->866|2709->891|2749->893|2781->899|2818->915|2850->920|2882->925|2909->931|2994->990|3055->1031|3088->1043|3117->1045|3146->1053|3182->1063|3205->1065|3241->1074|3278->1084|3322->1119|3372->1148|3402->1151|3425->1153|3464->1165|3569->1249|3606->1259|3629->1261|3663->1269|3686->1271|3718->1276|3741->1284|3759->1293|3799->1295|3831->1301|3873->1322|3905->1327|3937->1332|3964->1338|4049->1397|4110->1438|4143->1450|4172->1452|4201->1460|4237->1470|4260->1472|4296->1481|4333->1491|4377->1526|4427->1555|4457->1558|4480->1560|4517->1570|4612->1649|4652->1651|4691->1662|4723->1667|4755->1678|4785->1681|4808->1683|4849->1697|4903->1730|4942->1742|4965->1744|5006->1754|5043->1764|5066->1766|5100->1774|5123->1776|5155->1781|5194->1788|5224->1792|5246->1794|5278->1796|5307->1799|5329->1801
      |                  LINES: 10->1|11->2|16->3|21->4|22->5|22->5|22->5|24->7|26->9|26->9|26->9|27->10|27->10|27->10|27->10|27->10|29->12|29->12|30->13|30->13|30->13|30->13|30->13|31->14|31->14|31->14|32->15|32->15|33->16|33->16|33->16|33->16|33->16|35->18|35->18|35->18|36->19|37->20|37->20|39->22|39->22|39->22|39->22|39->22|39->23|39->23|39->23|40->24|40->24|41->25|41->25|41->25|42->26|42->26|42->26|42->26|42->26|43->27|43->27|44->28|44->28|44->28|44->28|44->28|44->28|45->29|45->29|46->30|46->30|47->31|47->31|48->32|49->34|49->34|49->34|50->35|50->35|51->36|51->36|51->36|52->37|52->37|52->37|52->37|52->37|53->38|53->38|54->39|54->39|54->39|54->39|54->39|54->39|55->40|55->40|55->40|56->41|56->41|56->41|56->41|56->41|57->42|57->42|58->43|58->43|59->44|60->45|60->45|61->46|61->46|62->47|63->48|64->49|64->49|65->50|67->52|67->52
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|sbt-routes-compiler""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|play/routes/compiler/static/twirl/reverseRouter.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package play.routes.compiler.static.twirl
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |/*1.2*/import play.routes.compiler._
      |/*2.2*/import play.routes.compiler.templates._
      |
      |object reverseRouter extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template7[RoutesSourceInfo,Option[String],Seq[String],Option[String],Seq[Route],Boolean,Route => Boolean,play.routes.compiler.ScalaFormat.Appendable] {
      |
      |  /**/
      |  def apply/*3.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], imports: Seq[String], packageName: Option[String], routes: Seq[Route], namespaceReverseRouter: Boolean, useInjector: Route => Boolean):play.routes.compiler.ScalaFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*4.1*/(${"\"" * 3}// @GENERATOR:play-routes-compiler
      |// @SOURCE:${"\"" * 3}),_display_(/*5.14*/sourceInfo/*5.24*/.source),format.raw/*5.31*/(${"\"" * 3}
      |
      |${"\"" * 3}),format.raw/*7.1*/(${"\"" * 3}import play.api.mvc.Call
      |
      |${"\"" * 3}),_display_(/*9.2*/for(i <- imports) yield /*9.19*/ {_display_(Seq[Any](format.raw/*9.21*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*10.1*/(${"\"" * 3}import ${"\"" * 3}),_display_(if(!i.startsWith("_root_."))/*10.37*/{_display_(Seq[Any](format.raw/*10.38*/(${"\"" * 3}_root_.${"\"" * 3})))} else {null} ),_display_(/*10.47*/i)))}),format.raw/*10.49*/(${"\"" * 3}
      |
      |${"\"" * 3}),_display_(/*12.2*/markLines(routes: _*)),format.raw/*12.23*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*13.2*/{packageName.map("package " + _ + " ").getOrElse("")}),_display_(if(packageName.isDefined)/*13.81*/{_display_(_display_(/*13.83*/ob))} else {null} ),format.raw/*13.86*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*14.2*/for((controller, routes) <- groupRoutesByController(routes)) yield /*14.62*/ {_display_(Seq[Any](format.raw/*14.64*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*15.4*/markLines(routes: _*)),format.raw/*15.25*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*16.3*/(${"\"" * 3}class Reverse${"\"" * 3}),_display_(/*16.17*/(controller.replace(".", "_"))),format.raw/*16.47*/(${"\"" * 3}(_prefix: => String) ${"\"" * 3}),_display_(/*16.69*/ob),format.raw/*16.71*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*17.5*/(${"\"" * 3}def _defaultPrefix: String = ${"\"" * 3}),_display_(/*17.35*/ob),format.raw/*17.37*/(${"\"" * 3}
      |      ${"\"" * 3}),format.raw/*18.7*/(${"\"" * 3}if (_prefix.endsWith("/")) "" else "/"
      |    ${"\"" * 3}),_display_(/*19.6*/cb),format.raw/*19.8*/(${"\"" * 3}
      |
      |  ${"\"" * 3}),_display_(/*21.4*/for(((method, _), routes) <- groupRoutesByMethod(routes)) yield /*21.61*/ {_display_(_display_(/*21.64*/routes/*21.70*/ match/*21.76*/ {/*22.3*/case Seq(route: Route) =>/*22.28*/ {_display_(Seq[Any](format.raw/*22.30*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*23.6*/markLines(route)),format.raw/*23.22*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*24.5*/(${"\"" * 3}def ${"\"" * 3}),_display_(/*24.10*/(method)),_display_(/*24.19*/(reverseSignature(routes))),format.raw/*24.45*/(${"\"" * 3}: Call = ${"\"" * 3}),_display_(/*24.55*/ob),format.raw/*24.57*/(${"\"" * 3}
      |      ${"\"" * 3}),_display_(/*25.8*/reverseRouteContext(route)),format.raw/*25.34*/(${"\"" * 3}
      |      ${"\"" * 3}),_display_(/*26.8*/reverseCall(route)),format.raw/*26.26*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*27.6*/cb),format.raw/*27.8*/(${"\"" * 3}
      |  ${"\"" * 3})))}/*29.3*/case _ =>/*29.12*/ {_display_(Seq[Any](format.raw/*29.14*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*30.6*/markLines(routes: _*)),format.raw/*30.27*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*31.5*/(${"\"" * 3}def ${"\"" * 3}),_display_(/*31.10*/(method)),_display_(/*31.19*/(reverseSignature(routes))),format.raw/*31.45*/(${"\"" * 3}: Call = ${"\"" * 3}),_display_(/*31.55*/ob),format.raw/*31.57*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*32.6*/defining(reverseParameters(routes))/*32.41*/ { params =>_display_(Seq[Any](format.raw/*32.53*/(${"\"" * 3}
      |      ${"\"" * 3}),format.raw/*33.7*/(${"\"" * 3}((${"\"" * 3}),_display_(/*33.10*/reverseMatchParameters(params, false)),format.raw/*33.47*/(${"\"" * 3}): @unchecked) match ${"\"" * 3}),_display_(/*33.70*/ob),format.raw/*33.72*/(${"\"" * 3}
      |      ${"\"" * 3}),_display_(/*34.8*/reverseUniqueConstraints(routes, params)/*34.48*/ { (route, parameters, parameterConstraints, localNames) =>_display_(Seq[Any](format.raw/*34.107*/(${"\"" * 3}
      |        ${"\"" * 3}),_display_(/*35.10*/markLines(route)),format.raw/*35.26*/(${"\"" * 3}
      |        case (${"\"" * 3}),_display_(/*36.16*/parameters),format.raw/*36.26*/(${"\"" * 3}) ${"\"" * 3}),_display_(/*36.29*/parameterConstraints),format.raw/*36.49*/(${"\"" * 3} ${"\"" * 3}),format.raw/*36.50*/(${"\"" * 3}=>
      |          ${"\"" * 3}),_display_(/*37.12*/reverseRouteContext(route)),format.raw/*37.38*/(${"\"" * 3}
      |          ${"\"" * 3}),_display_(/*38.12*/reverseCall(route, localNames)),format.raw/*38.42*/(${"\"" * 3}
      |      ${"\"" * 3})))}),format.raw/*39.8*/(${"\"" * 3}
      |      ${"\"" * 3}),_display_(/*40.8*/cb),format.raw/*40.10*/(${"\"" * 3}
      |    ${"\"" * 3})))}),format.raw/*41.6*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*42.6*/cb),format.raw/*42.8*/(${"\"" * 3}
      |  ${"\"" * 3})))}}))}),format.raw/*43.6*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*44.4*/cb),format.raw/*44.6*/(${"\"" * 3}
      |${"\"" * 3})))}),format.raw/*45.2*/(${"\"" * 3}
      |
      |${"\"" * 3}),_display_(if(packageName.isDefined)/*47.27*/{_display_(_display_(/*47.29*/cb))} else {null} ),format.raw/*47.32*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(sourceInfo:RoutesSourceInfo,pkg:Option[String],imports:Seq[String],packageName:Option[String],routes:Seq[Route],namespaceReverseRouter:Boolean,useInjector:Route => Boolean): play.routes.compiler.ScalaFormat.Appendable = apply(sourceInfo,pkg,imports,packageName,routes,namespaceReverseRouter,useInjector)
      |
      |  def f:((RoutesSourceInfo,Option[String],Seq[String],Option[String],Seq[Route],Boolean,Route => Boolean) => play.routes.compiler.ScalaFormat.Appendable) = (sourceInfo,pkg,imports,packageName,routes,namespaceReverseRouter,useInjector) => apply(sourceInfo,pkg,imports,packageName,routes,namespaceReverseRouter,useInjector)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/static/reverseRouter.scala.twirl
      |                  HASH: 1a93c6f62c3edcd003b83a8b433301f95f13836c
      |                  MATRIX: 292->1|329->32|797->73|1084->260|1157->309|1175->319|1202->326|1230->328|1282->355|1314->372|1353->374|1381->375|1444->411|1483->412|1535->421|1561->423|1590->426|1632->447|1660->449|1759->528|1789->530|1828->533|1856->535|1932->595|1972->597|2002->601|2044->622|2074->625|2115->639|2166->669|2215->691|2238->693|2270->698|2327->728|2350->730|2384->737|2454->781|2476->783|2507->788|2580->845|2611->848|2626->854|2641->860|2651->865|2685->890|2725->892|2757->898|2794->914|2826->919|2858->924|2887->933|2934->959|2971->969|2994->971|3028->979|3075->1005|3109->1013|3148->1031|3180->1037|3202->1039|3224->1046|3242->1055|3282->1057|3314->1063|3356->1084|3388->1089|3420->1094|3449->1103|3496->1129|3533->1139|3556->1141|3588->1147|3632->1182|3682->1194|3716->1201|3746->1204|3804->1241|3853->1264|3876->1266|3910->1274|3959->1314|4057->1373|4094->1383|4131->1399|4174->1415|4205->1425|4235->1428|4276->1448|4305->1449|4346->1463|4393->1489|4432->1501|4483->1531|4521->1539|4555->1547|4578->1549|4614->1555|4646->1561|4668->1563|4706->1569|4736->1573|4758->1575|4790->1577|4845->1605|4875->1607|4914->1610
      |                  LINES: 10->1|11->2|16->3|21->4|22->5|22->5|22->5|24->7|26->9|26->9|26->9|27->10|27->10|27->10|27->10|27->10|29->12|29->12|30->13|30->13|30->13|30->13|31->14|31->14|31->14|32->15|32->15|33->16|33->16|33->16|33->16|33->16|34->17|34->17|34->17|35->18|36->19|36->19|38->21|38->21|38->21|38->21|38->21|38->22|38->22|38->22|39->23|39->23|40->24|40->24|40->24|40->24|40->24|40->24|41->25|41->25|42->26|42->26|43->27|43->27|44->29|44->29|44->29|45->30|45->30|46->31|46->31|46->31|46->31|46->31|46->31|47->32|47->32|47->32|48->33|48->33|48->33|48->33|48->33|49->34|49->34|49->34|50->35|50->35|51->36|51->36|51->36|51->36|51->36|52->37|52->37|53->38|53->38|54->39|55->40|55->40|56->41|57->42|57->42|58->43|59->44|59->44|60->45|62->47|62->47|62->47
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|sbt-routes-compiler""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|play/routes/compiler/static/twirl/routesPrefix.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package play.routes.compiler.static.twirl
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |/*1.2*/import play.routes.compiler._
      |/*2.2*/import play.routes.compiler.templates._
      |
      |object routesPrefix extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template3[RoutesSourceInfo,Option[String],Route => Boolean,play.routes.compiler.ScalaFormat.Appendable] {
      |
      |  /**/
      |  def apply/*3.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], useInjector: Route => Boolean):play.routes.compiler.ScalaFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*4.1*/(${"\"" * 3}// @GENERATOR:play-routes-compiler
      |// @SOURCE:${"\"" * 3}),_display_(/*5.14*/sourceInfo/*5.24*/.source),format.raw/*5.31*/(${"\"" * 3}
      |
      |${"\"" * 3}),format.raw/*7.81*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*8.1*/(${"\"" * 3}package ${"\"" * 3}),_display_(/*8.10*/pkg/*8.13*/.getOrElse("_routes_")),format.raw/*8.35*/(${"\"" * 3} ${"\"" * 3}),_display_(/*8.37*/ob),format.raw/*8.39*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*9.3*/(${"\"" * 3}object RoutesPrefix ${"\"" * 3}),_display_(/*9.24*/ob),format.raw/*9.26*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*10.5*/(${"\"" * 3}private var _prefix: String = "/"
      |    def setPrefix(p: String): Unit = ${"\"" * 3}),_display_(/*11.39*/ob),format.raw/*11.41*/(${"\"" * 3}
      |      ${"\"" * 3}),format.raw/*12.7*/(${"\"" * 3}_prefix = p
      |    ${"\"" * 3}),_display_(/*13.6*/cb),format.raw/*13.8*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*14.5*/(${"\"" * 3}def prefix: String = _prefix
      |    val byNamePrefix: Function0[String] = ${"\"" * 3}),_display_(/*15.44*/ob),format.raw/*15.46*/(${"\"" * 3} ${"\"" * 3}),format.raw/*15.47*/(${"\"" * 3}() => prefix ${"\"" * 3}),_display_(/*15.61*/cb),format.raw/*15.63*/(${"\"" * 3}
      |  ${"\"" * 3}),_display_(/*16.4*/cb),format.raw/*16.6*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*17.2*/cb),format.raw/*17.4*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(sourceInfo:RoutesSourceInfo,pkg:Option[String],useInjector:Route => Boolean): play.routes.compiler.ScalaFormat.Appendable = apply(sourceInfo,pkg,useInjector)
      |
      |  def f:((RoutesSourceInfo,Option[String],Route => Boolean) => play.routes.compiler.ScalaFormat.Appendable) = (sourceInfo,pkg,useInjector) => apply(sourceInfo,pkg,useInjector)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/static/routesPrefix.scala.twirl
      |                  HASH: b27eb08a365e9195b7eada456bc93507ea8422c3
      |                  MATRIX: 292->1|329->32|750->73|933->156|1006->205|1024->215|1051->222|1080->304|1107->305|1142->314|1153->317|1195->339|1223->341|1245->343|1274->346|1321->367|1343->369|1375->374|1474->446|1497->448|1531->455|1574->472|1596->474|1628->479|1727->551|1750->553|1779->554|1820->568|1843->570|1873->574|1895->576|1923->578|1945->580
      |                  LINES: 10->1|11->2|16->3|21->4|22->5|22->5|22->5|24->7|25->8|25->8|25->8|25->8|25->8|25->8|26->9|26->9|26->9|27->10|28->11|28->11|29->12|30->13|30->13|31->14|32->15|32->15|32->15|32->15|32->15|33->16|33->16|34->17|34->17
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}