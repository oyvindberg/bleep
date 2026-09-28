
package play.routes.compiler.static.twirl

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
/*1.2*/import play.routes.compiler._
/*2.2*/import play.routes.compiler.templates._

object javascriptReverseRouter extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template7[RoutesSourceInfo,Option[String],Seq[String],Option[String],Seq[Route],Boolean,Route => Boolean,play.routes.compiler.ScalaFormat.Appendable] {

  /**/
  def apply/*3.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], imports: Seq[String], packageName: Option[String], routes: Seq[Route], namespaceReverseRouter: Boolean, useInjector: Route => Boolean):play.routes.compiler.ScalaFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*4.1*/("""// @GENERATOR:play-routes-compiler
// @SOURCE:"""),_display_(/*5.14*/sourceInfo/*5.24*/.source),format.raw/*5.31*/("""

"""),format.raw/*7.1*/("""import play.api.routing.JavaScriptReverseRoute

"""),_display_(/*9.2*/for(i <- imports) yield /*9.19*/ {_display_(Seq[Any](format.raw/*9.21*/("""
"""),format.raw/*10.1*/("""import """),_display_(if(!i.startsWith("_root_."))/*10.37*/{_display_(Seq[Any](format.raw/*10.38*/("""_root_.""")))} else {null} ),_display_(/*10.47*/i)))}),format.raw/*10.49*/("""

"""),_display_(/*12.2*/markLines(routes: _*)),format.raw/*12.23*/("""
"""),format.raw/*13.1*/("""package """),_display_(/*13.10*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*13.50*/("""javascript """),_display_(/*13.62*/ob),format.raw/*13.64*/("""
"""),_display_(/*14.2*/for((controller, routes) <- groupRoutesByController(routes)) yield /*14.62*/ {_display_(Seq[Any](format.raw/*14.64*/("""
  """),_display_(/*15.4*/markLines(routes: _*)),format.raw/*15.25*/("""
  """),format.raw/*16.3*/("""class Reverse"""),_display_(/*16.17*/(controller.replace(".", "_"))),format.raw/*16.47*/("""(_prefix: => String) """),_display_(/*16.69*/ob),format.raw/*16.71*/("""

    """),format.raw/*18.5*/("""def _defaultPrefix: String = """),_display_(/*18.35*/ob),format.raw/*18.37*/("""
      """),format.raw/*19.7*/("""if (_prefix.endsWith("/")) "" else "/"
    """),_display_(/*20.6*/cb),format.raw/*20.8*/("""

  """),_display_(/*22.4*/for(((method, _), routes) <- groupRoutesByMethod(routes)) yield /*22.61*/ {_display_(_display_(/*22.64*/routes/*22.70*/ match/*22.76*/ {/*23.3*/case Seq(route: Route) =>/*23.28*/ {_display_(Seq[Any](format.raw/*23.30*/("""
    """),_display_(/*24.6*/markLines(route)),format.raw/*24.22*/("""
    """),format.raw/*25.5*/("""def """),_display_(/*25.10*/method),format.raw/*25.16*/(""": JavaScriptReverseRoute = JavaScriptReverseRoute(
      """"),_display_(/*26.9*/{packageName.map(_ + ".").getOrElse("")}),_display_(/*26.50*/(controller)),format.raw/*26.62*/("""."""),_display_(/*26.64*/(method)),format.raw/*26.72*/("""",
      """),_display_(/*27.8*/tq),format.raw/*27.10*/("""
        """),format.raw/*28.9*/("""function("""),_display_(/*28.19*/reverseParametersJavascript(routes)/*28.54*/.map(_._1.name).mkString(",")),format.raw/*28.83*/(""") """),_display_(/*28.86*/ob),format.raw/*28.88*/("""
          """),_display_(/*29.12*/javascriptCall(route, reverseLocalNames(route, reverseParametersJavascript(routes)))),format.raw/*29.96*/("""
        """),_display_(/*30.10*/cb),format.raw/*30.12*/("""
      """),_display_(/*31.8*/tq),format.raw/*31.10*/("""
    """),format.raw/*32.5*/(""")
  """)))}/*34.3*/case _ =>/*34.12*/ {_display_(Seq[Any](format.raw/*34.14*/("""
    """),_display_(/*35.6*/markLines(routes: _*)),format.raw/*35.27*/("""
    """),format.raw/*36.5*/("""def """),_display_(/*36.10*/method),format.raw/*36.16*/(""": JavaScriptReverseRoute = JavaScriptReverseRoute(
      """"),_display_(/*37.9*/{packageName.map(_ + ".").getOrElse("")}),_display_(/*37.50*/(controller)),format.raw/*37.62*/("""."""),_display_(/*37.64*/(method)),format.raw/*37.72*/("""",
      """),_display_(/*38.8*/tq),format.raw/*38.10*/("""
        """),format.raw/*39.9*/("""function("""),_display_(/*39.19*/reverseParametersJavascript(routes)/*39.54*/.map(_._1.name).mkString(",")),format.raw/*39.83*/(""") """),_display_(/*39.86*/ob),format.raw/*39.88*/("""
        """),_display_(/*40.10*/for((route, localNames, constraints) <- javascriptCollectNonDeadRoutes(routes)) yield /*40.89*/ {_display_(Seq[Any](format.raw/*40.91*/("""
          """),format.raw/*41.11*/("""if ("""),_display_(/*41.16*/constraints),format.raw/*41.27*/(""") """),_display_(/*41.30*/ob),format.raw/*41.32*/("""
            """),_display_(/*42.14*/javascriptCall(route, localNames)),format.raw/*42.47*/("""
          """),_display_(/*43.12*/cb),format.raw/*43.14*/("""
        """)))}),format.raw/*44.10*/("""
        """),_display_(/*45.10*/cb),format.raw/*45.12*/("""
      """),_display_(/*46.8*/tq),format.raw/*46.10*/("""
    """),format.raw/*47.5*/(""")
  """)))}}))}),format.raw/*48.6*/("""
  """),_display_(/*49.4*/cb),format.raw/*49.6*/("""
""")))}),format.raw/*50.2*/("""

"""),_display_(/*52.2*/cb),format.raw/*52.4*/("""
"""))
      }
    }
  }

  def render(sourceInfo:RoutesSourceInfo,pkg:Option[String],imports:Seq[String],packageName:Option[String],routes:Seq[Route],namespaceReverseRouter:Boolean,useInjector:Route => Boolean): play.routes.compiler.ScalaFormat.Appendable = apply(sourceInfo,pkg,imports,packageName,routes,namespaceReverseRouter,useInjector)

  def f:((RoutesSourceInfo,Option[String],Seq[String],Option[String],Seq[Route],Boolean,Route => Boolean) => play.routes.compiler.ScalaFormat.Appendable) = (sourceInfo,pkg,imports,packageName,routes,namespaceReverseRouter,useInjector) => apply(sourceInfo,pkg,imports,packageName,routes,namespaceReverseRouter,useInjector)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/static/javascriptReverseRouter.scala.twirl
                  HASH: 5174e04f31bcd3249c9feb77dfac8d36453be87b
                  MATRIX: 330->1|367->32|845->73|1132->260|1205->309|1223->319|1250->326|1278->328|1352->377|1384->394|1423->396|1451->397|1514->433|1553->434|1605->443|1631->445|1660->448|1702->469|1730->470|1766->479|1827->519|1866->531|1889->533|1917->535|1993->595|2033->597|2063->601|2105->622|2135->625|2176->639|2227->669|2276->691|2299->693|2332->699|2389->729|2412->731|2446->738|2516->782|2538->784|2569->789|2642->846|2673->849|2688->855|2703->861|2713->866|2747->891|2787->893|2819->899|2856->915|2888->920|2920->925|2947->931|3032->990|3093->1031|3126->1043|3155->1045|3184->1053|3220->1063|3243->1065|3279->1074|3316->1084|3360->1119|3410->1148|3440->1151|3463->1153|3502->1165|3607->1249|3644->1259|3667->1261|3701->1269|3724->1271|3756->1276|3779->1284|3797->1293|3837->1295|3869->1301|3911->1322|3943->1327|3975->1332|4002->1338|4087->1397|4148->1438|4181->1450|4210->1452|4239->1460|4275->1470|4298->1472|4334->1481|4371->1491|4415->1526|4465->1555|4495->1558|4518->1560|4555->1570|4650->1649|4690->1651|4729->1662|4761->1667|4793->1678|4823->1681|4846->1683|4887->1697|4941->1730|4980->1742|5003->1744|5044->1754|5081->1764|5104->1766|5138->1774|5161->1776|5193->1781|5232->1788|5262->1792|5284->1794|5316->1796|5345->1799|5367->1801
                  LINES: 11->1|12->2|17->3|22->4|23->5|23->5|23->5|25->7|27->9|27->9|27->9|28->10|28->10|28->10|28->10|28->10|30->12|30->12|31->13|31->13|31->13|31->13|31->13|32->14|32->14|32->14|33->15|33->15|34->16|34->16|34->16|34->16|34->16|36->18|36->18|36->18|37->19|38->20|38->20|40->22|40->22|40->22|40->22|40->22|40->23|40->23|40->23|41->24|41->24|42->25|42->25|42->25|43->26|43->26|43->26|43->26|43->26|44->27|44->27|45->28|45->28|45->28|45->28|45->28|45->28|46->29|46->29|47->30|47->30|48->31|48->31|49->32|50->34|50->34|50->34|51->35|51->35|52->36|52->36|52->36|53->37|53->37|53->37|53->37|53->37|54->38|54->38|55->39|55->39|55->39|55->39|55->39|55->39|56->40|56->40|56->40|57->41|57->41|57->41|57->41|57->41|58->42|58->42|59->43|59->43|60->44|61->45|61->45|62->46|62->46|63->47|64->48|65->49|65->49|66->50|68->52|68->52
                  -- GENERATED --
              */
          