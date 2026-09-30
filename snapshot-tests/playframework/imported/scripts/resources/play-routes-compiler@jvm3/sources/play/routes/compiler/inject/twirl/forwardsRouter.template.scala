
package play.routes.compiler.inject.twirl

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
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
                  HASH: a49e2d6db36c44052b36d3f7a1d2302fa345c48f
                  MATRIX: 330->1|367->32|414->73|903->117|1174->288|1247->337|1265->347|1292->354|1321->357|1350->370|1390->372|1426->381|1452->383|1481->385|1607->485|1640->502|1680->504|1708->505|1771->541|1810->542|1862->551|1888->553|1917->555|2020->631|2052->647|2091->648|2121->652|2161->671|2191->675|2202->678|2229->684|2259->687|2271->690|2298->696|2331->698|2361->701|2434->747|2457->749|2488->753|2596->835|2628->851|2668->853|2701->860|2741->879|2773->885|2784->888|2811->894|2841->897|2853->900|2884->907|2914->910|2965->934|2997->950|3036->952|3048->955|3075->961|3109->964|3189->1017|3212->1019|3244->1024|3349->1103|3397->1130|3486->1192|3518->1208|3557->1210|3569->1213|3596->1219|3630->1222|3667->1233|3689->1235|3720->1239|3784->1276|3807->1278|3839->1283|3911->1329|3933->1331|3964->1335|4017->1361|4072->1400|4112->1402|4144->1408|4155->1411|4169->1416|4184->1422|4194->1431|4262->1490|4302->1492|4331->1494|4354->1497|4379->1502|4402->1504|4445->1520|4468->1523|4494->1528|4518->1530|4538->1539|4584->1576|4624->1578|4653->1580|4676->1583|4701->1588|4724->1590|4815->1653|4872->1688|4903->1691|4927->1694|4953->1699|4977->1701|4997->1710|5030->1734|5070->1736|5107->1746|5139->1757|5168->1759|5196->1766|5249->1792|5281->1794|5313->1799|5395->1853|5424->1854|5453->1855|5516->1890|5545->1891|5711->2031|5739->2032|5767->2033|5796->2036|5851->2075|5881->2077|5893->2080|5907->2085|5922->2091|5932->2094|6001->2154|6041->2156|6071->2160|6108->2176|6138->2179|6183->2197|6233->2226|6262->2227|6299->2237|6312->2241|6339->2247|6437->2318|6477->2320|6557->2356|6571->2360|6630->2397|6684->2424|6736->2455|6765->2456|6844->2508|6883->2509|6915->2514|6996->2553|7101->2637|7199->2709|7228->2722|7259->2725|7284->2727|7321->2738|7387->2784|7400->2788|7432->2799|7469->2810|7482->2814|7510->2821|7546->2831|7559->2835|7728->2982|7764->2992|7789->2996|7840->3020|7896->3055|7931->3064|8012->3124|8052->3137|8070->3146|8145->3200|8176->3214|8224->3253|8264->3255|8294->3259|8333->3277|8363->3280|8412->3302|8444->3313|8473->3315|8501->3322|8530->3323|8568->3334|8600->3345|8703->3420|8720->3427|8749->3434|8788->3441|8819->3445|8901->3500|8924->3502|8972->3523|9012->3525|9044->3530|9080->3549|9110->3551|9165->3590|9195->3592|9207->3595|9221->3600|9236->3606|9246->3611|9279->3635|9319->3637|9351->3643|9390->3661|9437->3681|9469->3692|9498->3694|9526->3701|9568->3728|9597->3748|9637->3750|9669->3756|9706->3772|9744->3783|9796->3814|9848->3840|9890->3861|9919->3863|9942->3865|9971->3867|10009->3884|10046->3894|10100->3927|10164->3964|10203->3965|10242->3976|10294->3985|10425->4094|10460->4103|10483->4105|10524->4116|10546->4118|10574->4120|10596->4122
                  LINES: 11->1|12->2|13->3|18->5|24->7|25->8|25->8|25->8|27->10|27->10|27->10|27->10|27->10|29->12|33->16|33->16|33->16|34->17|34->17|34->17|34->17|34->17|36->19|37->20|37->20|37->20|38->21|38->21|39->22|39->22|39->22|39->22|39->22|39->22|39->22|40->23|41->24|41->24|43->26|44->27|44->27|44->27|45->28|45->28|46->29|46->29|46->29|46->29|46->29|46->29|47->30|47->30|47->30|47->30|47->30|47->30|47->30|49->32|49->32|50->33|51->34|51->34|52->35|52->35|52->35|52->35|52->35|52->35|53->36|53->36|55->38|55->38|55->38|56->39|57->40|57->40|59->42|59->42|59->42|59->42|60->43|60->43|60->43|60->43|60->44|60->44|60->44|60->44|60->44|60->44|60->44|60->44|60->44|60->44|60->44|60->45|60->45|60->45|60->45|60->45|60->45|60->45|60->45|60->45|60->45|60->45|60->45|60->45|60->46|60->46|60->46|60->46|60->46|60->46|60->46|60->47|60->47|61->48|62->49|62->49|62->49|62->49|62->49|65->52|65->52|65->52|67->54|67->54|67->54|67->54|67->54|67->54|67->55|67->55|67->55|68->56|68->56|69->57|69->57|69->57|69->57|69->57|69->57|69->57|70->58|70->58|70->58|70->58|70->58|72->60|72->60|72->60|73->61|73->61|74->62|75->63|75->63|77->65|77->65|77->65|77->65|78->66|78->66|78->66|78->66|79->67|79->67|79->67|80->68|80->68|80->68|81->69|81->69|82->70|82->70|83->71|83->71|84->72|84->72|84->72|87->76|87->76|87->76|88->77|88->77|89->78|89->78|89->78|89->78|89->78|89->78|89->78|89->78|89->78|89->78|89->78|90->79|92->81|92->81|92->81|93->82|93->82|94->83|95->84|95->84|95->84|95->84|95->84|95->84|95->84|95->85|95->85|95->85|96->86|96->86|97->87|97->87|97->87|97->87|98->89|98->89|98->89|99->90|99->90|100->91|100->91|101->92|101->92|101->92|101->92|101->92|101->92|102->93|102->93|102->93|102->93|103->94|103->94|103->94|104->95|104->95|105->97|105->97|106->98|106->98
                  -- GENERATED --
              */
          