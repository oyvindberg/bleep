
package play.routes.compiler.static.twirl

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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
                  HASH: 687898b8e353064b2be1160a0827ec95c1d0d9bd
                  MATRIX: 292->1|329->32|807->73|1094->260|1167->309|1185->319|1212->326|1240->328|1314->377|1346->394|1385->396|1413->397|1476->433|1515->434|1567->443|1593->445|1622->448|1664->469|1692->470|1728->479|1789->519|1828->531|1851->533|1879->535|1955->595|1995->597|2025->601|2067->622|2097->625|2138->639|2189->669|2238->691|2261->693|2294->699|2351->729|2374->731|2408->738|2478->782|2500->784|2531->789|2604->846|2635->849|2650->855|2665->861|2675->866|2709->891|2749->893|2781->899|2818->915|2850->920|2882->925|2909->931|2994->990|3055->1031|3088->1043|3117->1045|3146->1053|3182->1063|3205->1065|3241->1074|3278->1084|3322->1119|3372->1148|3402->1151|3425->1153|3464->1165|3569->1249|3606->1259|3629->1261|3663->1269|3686->1271|3718->1276|3741->1284|3759->1293|3799->1295|3831->1301|3873->1322|3905->1327|3937->1332|3964->1338|4049->1397|4110->1438|4143->1450|4172->1452|4201->1460|4237->1470|4260->1472|4296->1481|4333->1491|4377->1526|4427->1555|4457->1558|4480->1560|4517->1570|4612->1649|4652->1651|4691->1662|4723->1667|4755->1678|4785->1681|4808->1683|4849->1697|4903->1730|4942->1742|4965->1744|5006->1754|5043->1764|5066->1766|5100->1774|5123->1776|5155->1781|5194->1788|5224->1792|5246->1794|5278->1796|5307->1799|5329->1801
                  LINES: 10->1|11->2|16->3|21->4|22->5|22->5|22->5|24->7|26->9|26->9|26->9|27->10|27->10|27->10|27->10|27->10|29->12|29->12|30->13|30->13|30->13|30->13|30->13|31->14|31->14|31->14|32->15|32->15|33->16|33->16|33->16|33->16|33->16|35->18|35->18|35->18|36->19|37->20|37->20|39->22|39->22|39->22|39->22|39->22|39->23|39->23|39->23|40->24|40->24|41->25|41->25|41->25|42->26|42->26|42->26|42->26|42->26|43->27|43->27|44->28|44->28|44->28|44->28|44->28|44->28|45->29|45->29|46->30|46->30|47->31|47->31|48->32|49->34|49->34|49->34|50->35|50->35|51->36|51->36|51->36|52->37|52->37|52->37|52->37|52->37|53->38|53->38|54->39|54->39|54->39|54->39|54->39|54->39|55->40|55->40|55->40|56->41|56->41|56->41|56->41|56->41|57->42|57->42|58->43|58->43|59->44|60->45|60->45|61->46|61->46|62->47|63->48|64->49|64->49|65->50|67->52|67->52
                  -- GENERATED --
              */
          