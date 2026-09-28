
package play.routes.compiler.static.twirl

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
/*1.2*/import play.routes.compiler._
/*2.2*/import play.routes.compiler.templates._

object reverseRouter extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template7[RoutesSourceInfo,Option[String],Seq[String],Option[String],Seq[Route],Boolean,Route => Boolean,play.routes.compiler.ScalaFormat.Appendable] {

  /**/
  def apply/*3.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], imports: Seq[String], packageName: Option[String], routes: Seq[Route], namespaceReverseRouter: Boolean, useInjector: Route => Boolean):play.routes.compiler.ScalaFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*4.1*/("""// @GENERATOR:play-routes-compiler
// @SOURCE:"""),_display_(/*5.14*/sourceInfo/*5.24*/.source),format.raw/*5.31*/("""

"""),format.raw/*7.1*/("""import play.api.mvc.Call

"""),_display_(/*9.2*/for(i <- imports) yield /*9.19*/ {_display_(Seq[Any](format.raw/*9.21*/("""
"""),format.raw/*10.1*/("""import """),_display_(if(!i.startsWith("_root_."))/*10.37*/{_display_(Seq[Any](format.raw/*10.38*/("""_root_.""")))} else {null} ),_display_(/*10.47*/i)))}),format.raw/*10.49*/("""

"""),_display_(/*12.2*/markLines(routes: _*)),format.raw/*12.23*/("""
"""),_display_(/*13.2*/{packageName.map("package " + _ + " ").getOrElse("")}),_display_(if(packageName.isDefined)/*13.81*/{_display_(_display_(/*13.83*/ob))} else {null} ),format.raw/*13.86*/("""
"""),_display_(/*14.2*/for((controller, routes) <- groupRoutesByController(routes)) yield /*14.62*/ {_display_(Seq[Any](format.raw/*14.64*/("""
  """),_display_(/*15.4*/markLines(routes: _*)),format.raw/*15.25*/("""
  """),format.raw/*16.3*/("""class Reverse"""),_display_(/*16.17*/(controller.replace(".", "_"))),format.raw/*16.47*/("""(_prefix: => String) """),_display_(/*16.69*/ob),format.raw/*16.71*/("""
    """),format.raw/*17.5*/("""def _defaultPrefix: String = """),_display_(/*17.35*/ob),format.raw/*17.37*/("""
      """),format.raw/*18.7*/("""if (_prefix.endsWith("/")) "" else "/"
    """),_display_(/*19.6*/cb),format.raw/*19.8*/("""

  """),_display_(/*21.4*/for(((method, _), routes) <- groupRoutesByMethod(routes)) yield /*21.61*/ {_display_(_display_(/*21.64*/routes/*21.70*/ match/*21.76*/ {/*22.3*/case Seq(route: Route) =>/*22.28*/ {_display_(Seq[Any](format.raw/*22.30*/("""
    """),_display_(/*23.6*/markLines(route)),format.raw/*23.22*/("""
    """),format.raw/*24.5*/("""def """),_display_(/*24.10*/(method)),_display_(/*24.19*/(reverseSignature(routes))),format.raw/*24.45*/(""": Call = """),_display_(/*24.55*/ob),format.raw/*24.57*/("""
      """),_display_(/*25.8*/reverseRouteContext(route)),format.raw/*25.34*/("""
      """),_display_(/*26.8*/reverseCall(route)),format.raw/*26.26*/("""
    """),_display_(/*27.6*/cb),format.raw/*27.8*/("""
  """)))}/*29.3*/case _ =>/*29.12*/ {_display_(Seq[Any](format.raw/*29.14*/("""
    """),_display_(/*30.6*/markLines(routes: _*)),format.raw/*30.27*/("""
    """),format.raw/*31.5*/("""def """),_display_(/*31.10*/(method)),_display_(/*31.19*/(reverseSignature(routes))),format.raw/*31.45*/(""": Call = """),_display_(/*31.55*/ob),format.raw/*31.57*/("""
    """),_display_(/*32.6*/defining(reverseParameters(routes))/*32.41*/ { params =>_display_(Seq[Any](format.raw/*32.53*/("""
      """),format.raw/*33.7*/("""(("""),_display_(/*33.10*/reverseMatchParameters(params, false)),format.raw/*33.47*/("""): @unchecked) match """),_display_(/*33.70*/ob),format.raw/*33.72*/("""
      """),_display_(/*34.8*/reverseUniqueConstraints(routes, params)/*34.48*/ { (route, parameters, parameterConstraints, localNames) =>_display_(Seq[Any](format.raw/*34.107*/("""
        """),_display_(/*35.10*/markLines(route)),format.raw/*35.26*/("""
        case ("""),_display_(/*36.16*/parameters),format.raw/*36.26*/(""") """),_display_(/*36.29*/parameterConstraints),format.raw/*36.49*/(""" """),format.raw/*36.50*/("""=>
          """),_display_(/*37.12*/reverseRouteContext(route)),format.raw/*37.38*/("""
          """),_display_(/*38.12*/reverseCall(route, localNames)),format.raw/*38.42*/("""
      """)))}),format.raw/*39.8*/("""
      """),_display_(/*40.8*/cb),format.raw/*40.10*/("""
    """)))}),format.raw/*41.6*/("""
    """),_display_(/*42.6*/cb),format.raw/*42.8*/("""
  """)))}}))}),format.raw/*43.6*/("""
  """),_display_(/*44.4*/cb),format.raw/*44.6*/("""
""")))}),format.raw/*45.2*/("""

"""),_display_(if(packageName.isDefined)/*47.27*/{_display_(_display_(/*47.29*/cb))} else {null} ),format.raw/*47.32*/("""
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
                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/static/reverseRouter.scala.twirl
                  HASH: 1a93c6f62c3edcd003b83a8b433301f95f13836c
                  MATRIX: 292->1|329->32|797->73|1084->260|1157->309|1175->319|1202->326|1230->328|1282->355|1314->372|1353->374|1381->375|1444->411|1483->412|1535->421|1561->423|1590->426|1632->447|1660->449|1759->528|1789->530|1828->533|1856->535|1932->595|1972->597|2002->601|2044->622|2074->625|2115->639|2166->669|2215->691|2238->693|2270->698|2327->728|2350->730|2384->737|2454->781|2476->783|2507->788|2580->845|2611->848|2626->854|2641->860|2651->865|2685->890|2725->892|2757->898|2794->914|2826->919|2858->924|2887->933|2934->959|2971->969|2994->971|3028->979|3075->1005|3109->1013|3148->1031|3180->1037|3202->1039|3224->1046|3242->1055|3282->1057|3314->1063|3356->1084|3388->1089|3420->1094|3449->1103|3496->1129|3533->1139|3556->1141|3588->1147|3632->1182|3682->1194|3716->1201|3746->1204|3804->1241|3853->1264|3876->1266|3910->1274|3959->1314|4057->1373|4094->1383|4131->1399|4174->1415|4205->1425|4235->1428|4276->1448|4305->1449|4346->1463|4393->1489|4432->1501|4483->1531|4521->1539|4555->1547|4578->1549|4614->1555|4646->1561|4668->1563|4706->1569|4736->1573|4758->1575|4790->1577|4845->1605|4875->1607|4914->1610
                  LINES: 10->1|11->2|16->3|21->4|22->5|22->5|22->5|24->7|26->9|26->9|26->9|27->10|27->10|27->10|27->10|27->10|29->12|29->12|30->13|30->13|30->13|30->13|31->14|31->14|31->14|32->15|32->15|33->16|33->16|33->16|33->16|33->16|34->17|34->17|34->17|35->18|36->19|36->19|38->21|38->21|38->21|38->21|38->21|38->22|38->22|38->22|39->23|39->23|40->24|40->24|40->24|40->24|40->24|40->24|41->25|41->25|42->26|42->26|43->27|43->27|44->29|44->29|44->29|45->30|45->30|46->31|46->31|46->31|46->31|46->31|46->31|47->32|47->32|47->32|48->33|48->33|48->33|48->33|48->33|49->34|49->34|49->34|50->35|50->35|51->36|51->36|51->36|51->36|51->36|52->37|52->37|53->38|53->38|54->39|55->40|55->40|56->41|57->42|57->42|58->43|59->44|59->44|60->45|62->47|62->47|62->47
                  -- GENERATED --
              */
          