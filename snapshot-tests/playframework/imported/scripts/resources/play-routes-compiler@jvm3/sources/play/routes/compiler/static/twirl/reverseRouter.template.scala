
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
                  HASH: 58b6f1cd16efa24bc0ecefdf43065878bf10360e
                  MATRIX: 330->1|367->32|835->73|1122->260|1195->309|1213->319|1240->326|1268->328|1320->355|1352->372|1391->374|1419->375|1482->411|1521->412|1573->421|1599->423|1628->426|1670->447|1698->449|1797->528|1827->530|1866->533|1894->535|1970->595|2010->597|2040->601|2082->622|2112->625|2153->639|2204->669|2253->691|2276->693|2308->698|2365->728|2388->730|2422->737|2492->781|2514->783|2545->788|2618->845|2649->848|2664->854|2679->860|2689->865|2723->890|2763->892|2795->898|2832->914|2864->919|2896->924|2925->933|2972->959|3009->969|3032->971|3066->979|3113->1005|3147->1013|3186->1031|3218->1037|3240->1039|3262->1046|3280->1055|3320->1057|3352->1063|3394->1084|3426->1089|3458->1094|3487->1103|3534->1129|3571->1139|3594->1141|3626->1147|3670->1182|3720->1194|3754->1201|3784->1204|3842->1241|3891->1264|3914->1266|3948->1274|3997->1314|4095->1373|4132->1383|4169->1399|4212->1415|4243->1425|4273->1428|4314->1448|4343->1449|4384->1463|4431->1489|4470->1501|4521->1531|4559->1539|4593->1547|4616->1549|4652->1555|4684->1561|4706->1563|4744->1569|4774->1573|4796->1575|4828->1577|4883->1605|4913->1607|4952->1610
                  LINES: 11->1|12->2|17->3|22->4|23->5|23->5|23->5|25->7|27->9|27->9|27->9|28->10|28->10|28->10|28->10|28->10|30->12|30->12|31->13|31->13|31->13|31->13|32->14|32->14|32->14|33->15|33->15|34->16|34->16|34->16|34->16|34->16|35->17|35->17|35->17|36->18|37->19|37->19|39->21|39->21|39->21|39->21|39->21|39->22|39->22|39->22|40->23|40->23|41->24|41->24|41->24|41->24|41->24|41->24|42->25|42->25|43->26|43->26|44->27|44->27|45->29|45->29|45->29|46->30|46->30|47->31|47->31|47->31|47->31|47->31|47->31|48->32|48->32|48->32|49->33|49->33|49->33|49->33|49->33|50->34|50->34|50->34|51->35|51->35|52->36|52->36|52->36|52->36|52->36|53->37|53->37|54->38|54->38|55->39|56->40|56->40|57->41|58->42|58->42|59->43|60->44|60->44|61->45|63->47|63->47|63->47
                  -- GENERATED --
              */
          