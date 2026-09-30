
package play.routes.compiler.inject.twirl

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
/*1.2*/import play.routes.compiler._
/*2.2*/import play.routes.compiler.templates._
/*3.2*/import InjectedRoutesGenerator.Dependency

object forwardsRouter extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template6[RoutesSourceInfo,Option[String],Seq[String],Seq[Dependency[Rule]],Seq[Dependency[Rule]],Seq[Dependency[Include]],play.routes.compiler.ScalaFormat.Appendable] {

  /**/
  def apply/*5.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], imports: Seq[String],
  deps: Seq[Dependency[Rule]], rules: Seq[Dependency[Rule]], includes: Seq[Dependency[Include]]):play.routes.compiler.ScalaFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*7.1*/("""// @GENERATOR:play-routes-compiler
// @SOURCE:"""),_display_(/*8.14*/sourceInfo/*8.24*/.source),format.raw/*8.31*/("""

"""),_display_(/*10.2*/for(p <- pkg) yield /*10.15*/ {_display_(Seq[Any](format.raw/*10.17*/("""package """),_display_(/*10.26*/p)))}),format.raw/*10.28*/("""

"""),format.raw/*12.1*/("""import play.core.routing._
import play.core.routing.HandlerInvokerFactory._

import play.api.mvc._
"""),_display_(/*16.2*/for(i <- imports) yield /*16.19*/ {_display_(Seq[Any](format.raw/*16.21*/("""
"""),format.raw/*17.1*/("""import """),_display_(if(!i.startsWith("_root_."))/*17.37*/{_display_(Seq[Any](format.raw/*17.38*/("""_root_.""")))} else {null} ),_display_(/*17.47*/i)))}),format.raw/*17.49*/("""

"""),format.raw/*19.1*/("""class Routes(
  override val errorHandler: play.api.http.HttpErrorHandler, """),_display_(/*20.63*/for(dep <- deps) yield /*20.79*/{_display_(Seq[Any](format.raw/*20.80*/("""
  """),_display_(/*21.4*/markLines(dep.rule)),format.raw/*21.23*/("""
  """),_display_(/*22.4*/dep/*22.7*/.ident),format.raw/*22.13*/(""": """),_display_(/*22.16*/dep/*22.19*/.clazz),format.raw/*22.25*/(""",""")))}),format.raw/*22.27*/("""
  """),format.raw/*23.3*/("""val prefix: String
) extends GeneratedRouter """),_display_(/*24.28*/ob),format.raw/*24.30*/("""

  """),format.raw/*26.3*/("""@jakarta.inject.Inject()
  def this(errorHandler: play.api.http.HttpErrorHandler"""),_display_(/*27.57*/for(dep <- deps) yield /*27.73*/ {_display_(Seq[Any](format.raw/*27.75*/(""",
    """),_display_(/*28.6*/markLines(dep.rule)),format.raw/*28.25*/("""
    """),_display_(/*29.6*/dep/*29.9*/.ident),format.raw/*29.15*/(""": """),_display_(/*29.18*/dep/*29.21*/.clazz)))}),format.raw/*29.28*/("""
  """),format.raw/*30.3*/(""") = this(errorHandler, """),_display_(/*30.27*/for(dep <- deps) yield /*30.43*/{_display_(Seq[Any](_display_(/*30.45*/dep/*30.48*/.ident),format.raw/*30.54*/(""", """)))}),format.raw/*30.57*/(""""/")

  def withPrefix(addPrefix: String): Routes = """),_display_(/*32.48*/ob),format.raw/*32.50*/("""
    """),format.raw/*33.5*/("""val prefix = play.api.routing.Router.concatPrefix(addPrefix, this.prefix)
    """),_display_(/*34.6*/(pkg.getOrElse("_routes_"))),format.raw/*34.33*/(""".RoutesPrefix.setPrefix(prefix)
    new Routes(errorHandler, """),_display_(/*35.31*/for(dep <- deps) yield /*35.47*/{_display_(Seq[Any](_display_(/*35.49*/dep/*35.52*/.ident),format.raw/*35.58*/(""", """)))}),format.raw/*35.61*/("""prefix)
  """),_display_(/*36.4*/cb),format.raw/*36.6*/("""

  """),format.raw/*38.3*/("""private val defaultPrefix: String = """),_display_(/*38.40*/ob),format.raw/*38.42*/("""
    """),format.raw/*39.5*/("""if (this.prefix.endsWith("/")) "" else "/"
  """),_display_(/*40.4*/cb),format.raw/*40.6*/("""

  """),format.raw/*42.3*/("""def documentation = List("""),_display_(/*42.29*/for((dep, index) <- rules.zipWithIndex) yield /*42.68*/ {_display_(Seq[Any](format.raw/*42.70*/("""
    """),_display_(/*43.6*/dep/*43.9*/.rule/*43.14*/ match/*43.20*/ {/*44.7*/case Route(verb, path, call, _, _) if path.parts.isEmpty =>/*44.66*/ {_display_(Seq[Any](format.raw/*44.68*/("""("""),_display_(/*44.70*/tq),_display_(/*44.73*/verb),_display_(/*44.78*/tq),format.raw/*44.80*/(""", this.prefix, """),_display_(/*44.96*/tq),_display_(/*44.99*/call),_display_(/*44.104*/tq),format.raw/*44.106*/(""")""")))}/*45.7*/case Route(verb, path, call, _, _) =>/*45.44*/ {_display_(Seq[Any](format.raw/*45.46*/("""("""),_display_(/*45.48*/tq),_display_(/*45.51*/verb),_display_(/*45.56*/tq),format.raw/*45.58*/(""", this.prefix + (if(this.prefix.endsWith("/")) "" else "/") + """),_display_(/*45.121*/encodeStringConstant(path.toString)),format.raw/*45.156*/(""", """),_display_(/*45.159*/tq),_display_(/*45.162*/call),_display_(/*45.167*/tq),format.raw/*45.169*/(""")""")))}/*46.7*/case include: Include =>/*46.31*/ {_display_(Seq[Any](format.raw/*46.33*/("""prefixed_"""),_display_(/*46.43*/(dep.ident)),format.raw/*46.54*/("""_"""),_display_(/*46.56*/(index)),format.raw/*46.63*/(""".router.documentation""")))}}),format.raw/*47.4*/(""",""")))}),format.raw/*47.6*/("""
    """),format.raw/*48.5*/("""Nil
  ).foldLeft(Seq.empty[(String, String, String)]) """),format.raw/*49.51*/("""{"""),format.raw/*49.52*/(""" """),format.raw/*49.53*/("""(s,e) => e.asInstanceOf[Any] match """),format.raw/*49.88*/("""{"""),format.raw/*49.89*/("""
    case r @ (_,_,_) => s :+ r.asInstanceOf[(String, String, String)]
    case l => s ++ l.asInstanceOf[List[(String, String, String)]]
  """),format.raw/*52.3*/("""}"""),format.raw/*52.4*/("""}"""),format.raw/*52.5*/("""

"""),_display_(/*54.2*/for((dep, index) <- rules.zipWithIndex) yield /*54.41*/{_display_(_display_(/*54.43*/dep/*54.46*/.rule/*54.51*/ match/*54.57*/ {/*55.1*/case route @ Route(verb, path, call, comments, modifiers) =>/*55.61*/ {_display_(Seq[Any](format.raw/*55.63*/("""
  """),_display_(/*56.4*/markLines(route)),format.raw/*56.20*/("""
  """),format.raw/*57.3*/("""private lazy val """),_display_(/*57.21*/routeIdentifier(route, index)),format.raw/*57.50*/(""" """),format.raw/*57.51*/("""= Route(""""),_display_(/*57.61*/verb/*57.65*/.value),format.raw/*57.71*/("""",
    PathPattern(List(StaticPart(this.prefix)"""),_display_(if(path.parts.nonEmpty)/*58.69*/ {_display_(Seq[Any](format.raw/*58.71*/(""", StaticPart(this.defaultPrefix), """)))} else {null} ),_display_(/*58.107*/path/*58.111*/.parts.map(_.toString).mkString(", ")),format.raw/*58.148*/("""))
  )
  private lazy val """),_display_(/*60.21*/invokerIdentifier(route, index)),format.raw/*60.52*/(""" """),format.raw/*60.53*/("""= createInvoker(
    """),_display_(if(route.call.passJavaRequest)/*61.36*/{_display_(Seq[Any](format.raw/*61.37*/("""
    """),format.raw/*62.5*/("""(req:play.mvc.Http.Request) =>
      """)))} else {null} ),_display_(/*63.9*/injectedControllerMethodCall(route, dep.ident, p => s"fakeValue[${p.typeNameReal}]")),format.raw/*63.93*/(""",
    play.api.routing.HandlerDef(this.getClass.getClassLoader,
      """"),_display_(/*65.9*/for(p <- pkg) yield /*65.22*/ {_display_(_display_(/*65.25*/p))}),format.raw/*65.27*/("""",
      """"),_display_(/*66.9*/{call.packageName.map(_ + ".").getOrElse("")}),_display_(/*66.55*/call/*66.59*/.controller),format.raw/*66.70*/("""",
      """"),_display_(/*67.9*/call/*67.13*/.method),format.raw/*67.20*/("""",
      """),_display_(/*68.8*/call/*68.12*/.parameters.filterNot(_.isEmpty).map(params => params.map("classOf[" + _.typeNameReal + "]").mkString(", ")).map("Seq(" + _ + ")").getOrElse("Nil")),format.raw/*68.159*/(""",
      """"),_display_(/*69.9*/verb),format.raw/*69.13*/("""",
      this.prefix + """),_display_(/*70.22*/encodeStringConstant(path.toString)),format.raw/*70.57*/(""",
      """),_display_(/*71.8*/encodeStringConstant(comments.map(_.comment).mkString("\n"))),format.raw/*71.68*/(""",
      Seq("""),_display_(/*72.12*/modifiers/*72.21*/.map(_.value).map(encodeStringConstant).mkString(", ")),format.raw/*72.75*/(""")
    )
  )
""")))}/*76.1*/case include @ Include(path, router) =>/*76.40*/ {_display_(Seq[Any](format.raw/*76.42*/("""
  """),_display_(/*77.4*/markLines(include)),format.raw/*77.22*/("""
  """),format.raw/*78.3*/("""private val prefixed_"""),_display_(/*78.25*/(dep.ident)),format.raw/*78.36*/("""_"""),_display_(/*78.38*/(index)),format.raw/*78.45*/(""" """),format.raw/*78.46*/("""= Include("""),_display_(/*78.57*/(dep.ident)),format.raw/*78.68*/(""".withPrefix(this.prefix + (if (this.prefix.endsWith("/")) "" else "/") + """"),_display_(/*78.143*/include/*78.150*/.prefix),format.raw/*78.157*/(""""))
""")))}}))}),format.raw/*79.4*/("""

  """),format.raw/*81.3*/("""def routes: PartialFunction[RequestHeader, Handler] = """),_display_(/*81.58*/ob),format.raw/*81.60*/("""
  """),_display_(if(rules.isEmpty)/*82.21*/ {_display_(Seq[Any](format.raw/*82.23*/("""
    """),format.raw/*83.5*/("""Map.empty
  """)))}else/*84.10*/{_display_(_display_(/*84.12*/for((dep, index) <- rules.zipWithIndex) yield /*84.51*/{_display_(_display_(/*84.53*/dep/*84.56*/.rule/*84.61*/ match/*84.67*/ {/*85.3*/case include: Include =>/*85.27*/ {_display_(Seq[Any](format.raw/*85.29*/("""
    """),_display_(/*86.6*/markLines(include)),format.raw/*86.24*/("""
    case prefixed_"""),_display_(/*87.20*/(dep.ident)),format.raw/*87.31*/("""_"""),_display_(/*87.33*/(index)),format.raw/*87.40*/("""(handler) => handler
  """)))}/*89.3*/case route: Route =>/*89.23*/ {_display_(Seq[Any](format.raw/*89.25*/("""
    """),_display_(/*90.6*/markLines(route)),format.raw/*90.22*/("""
    case """),_display_(/*91.11*/(routeIdentifier(route, index))),format.raw/*91.42*/("""(params@_) =>
      call"""),_display_(/*92.12*/(routeBinding(route))),format.raw/*92.33*/(""" """),_display_(/*92.35*/ob),format.raw/*92.37*/(""" """),_display_(/*92.39*/localNames(route)),format.raw/*92.56*/("""
        """),_display_(/*93.10*/(invokerIdentifier(route, index))),format.raw/*93.43*/(""".call("""),_display_(if(route.call.passJavaRequest)/*93.80*/{_display_(Seq[Any](format.raw/*93.81*/("""
          """),format.raw/*94.11*/("""req => """)))} else {null} ),_display_(/*94.20*/injectedControllerMethodCall(route, dep.ident, x => if (x.isJavaRequest) "req" else safeKeyword(x.nameClean))),format.raw/*94.129*/(""")
      """),_display_(/*95.8*/cb),format.raw/*95.10*/("""
  """)))}}))}))}),_display_(/*97.7*/cb),format.raw/*97.9*/("""
"""),_display_(/*98.2*/cb),format.raw/*98.4*/("""
"""))
      }
    }
  }

  def render(sourceInfo:RoutesSourceInfo,pkg:Option[String],imports:Seq[String],deps:Seq[Dependency[Rule]],rules:Seq[Dependency[Rule]],includes:Seq[Dependency[Include]]): play.routes.compiler.ScalaFormat.Appendable = apply(sourceInfo,pkg,imports,deps,rules,includes)

  def f:((RoutesSourceInfo,Option[String],Seq[String],Seq[Dependency[Rule]],Seq[Dependency[Rule]],Seq[Dependency[Include]]) => play.routes.compiler.ScalaFormat.Appendable) = (sourceInfo,pkg,imports,deps,rules,includes) => apply(sourceInfo,pkg,imports,deps,rules,includes)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/inject/forwardsRouter.scala.twirl
                  HASH: 82cbbda630f8e8124b671e803aa70eada1c70f72
                  MATRIX: 292->1|329->32|376->73|865->117|1136->288|1209->337|1227->347|1254->354|1283->357|1312->370|1352->372|1388->381|1414->383|1443->385|1569->485|1602->502|1642->504|1670->505|1733->541|1772->542|1824->551|1850->553|1879->555|1982->631|2014->647|2053->648|2083->652|2123->671|2153->675|2164->678|2191->684|2221->687|2233->690|2260->696|2293->698|2323->701|2396->747|2419->749|2450->753|2558->835|2590->851|2630->853|2663->860|2703->879|2735->885|2746->888|2773->894|2803->897|2815->900|2846->907|2876->910|2927->934|2959->950|2998->952|3010->955|3037->961|3071->964|3151->1017|3174->1019|3206->1024|3311->1103|3359->1130|3448->1192|3480->1208|3519->1210|3531->1213|3558->1219|3592->1222|3629->1233|3651->1235|3682->1239|3746->1276|3769->1278|3801->1283|3873->1329|3895->1331|3926->1335|3979->1361|4034->1400|4074->1402|4106->1408|4117->1411|4131->1416|4146->1422|4156->1431|4224->1490|4264->1492|4293->1494|4316->1497|4341->1502|4364->1504|4407->1520|4430->1523|4456->1528|4480->1530|4500->1539|4546->1576|4586->1578|4615->1580|4638->1583|4663->1588|4686->1590|4777->1653|4834->1688|4865->1691|4889->1694|4915->1699|4939->1701|4959->1710|4992->1734|5032->1736|5069->1746|5101->1757|5130->1759|5158->1766|5211->1792|5243->1794|5275->1799|5357->1853|5386->1854|5415->1855|5478->1890|5507->1891|5673->2031|5701->2032|5729->2033|5758->2036|5813->2075|5843->2077|5855->2080|5869->2085|5884->2091|5894->2094|5963->2154|6003->2156|6033->2160|6070->2176|6100->2179|6145->2197|6195->2226|6224->2227|6261->2237|6274->2241|6301->2247|6399->2318|6439->2320|6519->2356|6533->2360|6592->2397|6646->2424|6698->2455|6727->2456|6806->2508|6845->2509|6877->2514|6958->2553|7063->2637|7161->2709|7190->2722|7221->2725|7246->2727|7283->2738|7349->2784|7362->2788|7394->2799|7431->2810|7444->2814|7472->2821|7508->2831|7521->2835|7690->2982|7726->2992|7751->2996|7802->3020|7858->3055|7893->3064|7974->3124|8014->3137|8032->3146|8107->3200|8138->3214|8186->3253|8226->3255|8256->3259|8295->3277|8325->3280|8374->3302|8406->3313|8435->3315|8463->3322|8492->3323|8530->3334|8562->3345|8665->3420|8682->3427|8711->3434|8750->3441|8781->3445|8863->3500|8886->3502|8934->3523|8974->3525|9006->3530|9042->3549|9072->3551|9127->3590|9157->3592|9169->3595|9183->3600|9198->3606|9208->3611|9241->3635|9281->3637|9313->3643|9352->3661|9399->3681|9431->3692|9460->3694|9488->3701|9530->3728|9559->3748|9599->3750|9631->3756|9668->3772|9706->3783|9758->3814|9810->3840|9852->3861|9881->3863|9904->3865|9933->3867|9971->3884|10008->3894|10062->3927|10126->3964|10165->3965|10204->3976|10256->3985|10387->4094|10422->4103|10445->4105|10486->4116|10508->4118|10536->4120|10558->4122
                  LINES: 10->1|11->2|12->3|17->5|23->7|24->8|24->8|24->8|26->10|26->10|26->10|26->10|26->10|28->12|32->16|32->16|32->16|33->17|33->17|33->17|33->17|33->17|35->19|36->20|36->20|36->20|37->21|37->21|38->22|38->22|38->22|38->22|38->22|38->22|38->22|39->23|40->24|40->24|42->26|43->27|43->27|43->27|44->28|44->28|45->29|45->29|45->29|45->29|45->29|45->29|46->30|46->30|46->30|46->30|46->30|46->30|46->30|48->32|48->32|49->33|50->34|50->34|51->35|51->35|51->35|51->35|51->35|51->35|52->36|52->36|54->38|54->38|54->38|55->39|56->40|56->40|58->42|58->42|58->42|58->42|59->43|59->43|59->43|59->43|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->44|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->45|59->46|59->46|59->46|59->46|59->46|59->46|59->46|59->47|59->47|60->48|61->49|61->49|61->49|61->49|61->49|64->52|64->52|64->52|66->54|66->54|66->54|66->54|66->54|66->54|66->55|66->55|66->55|67->56|67->56|68->57|68->57|68->57|68->57|68->57|68->57|68->57|69->58|69->58|69->58|69->58|69->58|71->60|71->60|71->60|72->61|72->61|73->62|74->63|74->63|76->65|76->65|76->65|76->65|77->66|77->66|77->66|77->66|78->67|78->67|78->67|79->68|79->68|79->68|80->69|80->69|81->70|81->70|82->71|82->71|83->72|83->72|83->72|86->76|86->76|86->76|87->77|87->77|88->78|88->78|88->78|88->78|88->78|88->78|88->78|88->78|88->78|88->78|88->78|89->79|91->81|91->81|91->81|92->82|92->82|93->83|94->84|94->84|94->84|94->84|94->84|94->84|94->84|94->85|94->85|94->85|95->86|95->86|96->87|96->87|96->87|96->87|97->89|97->89|97->89|98->90|98->90|99->91|99->91|100->92|100->92|100->92|100->92|100->92|100->92|101->93|101->93|101->93|101->93|102->94|102->94|102->94|103->95|103->95|104->97|104->97|105->98|105->98
                  -- GENERATED --
              */
          