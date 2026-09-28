
package play.routes.compiler.static.twirl

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
/*1.2*/import play.routes.compiler._
/*2.2*/import play.routes.compiler.templates._

object javaWrappers extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template5[RoutesSourceInfo,Option[String],Option[String],Seq[String],Boolean,play.routes.compiler.ScalaFormat.Appendable] {

  /**/
  def apply/*3.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], packageName: Option[String], controllers: Seq[String], jsReverseRouter: Boolean):play.routes.compiler.ScalaFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*4.1*/("""// @GENERATOR:play-routes-compiler
// @SOURCE:"""),_display_(/*5.14*/sourceInfo/*5.24*/.source),format.raw/*5.31*/("""

"""),_display_(/*7.2*/{packageName.map("package " + _ + ";").getOrElse("")}),format.raw/*7.55*/("""

"""),format.raw/*9.1*/("""import """),_display_(/*9.9*/(pkg.getOrElse("_routes_"))),format.raw/*9.36*/(""".RoutesPrefix;

public class routes """),_display_(/*11.22*/ob),format.raw/*11.24*/("""
  """),_display_(/*12.4*/for(controller <- controllers) yield /*12.34*/ {_display_(Seq[Any](format.raw/*12.36*/("""
  """),format.raw/*13.3*/("""public static final """),_display_(/*13.24*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*13.64*/("""Reverse"""),_display_(/*13.72*/controller),format.raw/*13.82*/(""" """),_display_(/*13.84*/controller),format.raw/*13.94*/(""" """),format.raw/*13.95*/("""= new """),_display_(/*13.102*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*13.142*/("""Reverse"""),_display_(/*13.150*/(controller)),format.raw/*13.162*/("""(RoutesPrefix.byNamePrefix());""")))}),format.raw/*13.193*/("""
"""),_display_(if(jsReverseRouter)/*14.21*/ {_display_(Seq[Any](format.raw/*14.23*/("""
  """),format.raw/*15.3*/("""public static class javascript """),_display_(/*15.35*/ob),format.raw/*15.37*/("""
    """),_display_(/*16.6*/for(controller <- controllers) yield /*16.36*/ {_display_(Seq[Any](format.raw/*16.38*/("""
    """),format.raw/*17.5*/("""public static final """),_display_(/*17.26*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*17.66*/("""javascript.Reverse"""),_display_(/*17.85*/controller),format.raw/*17.95*/(""" """),_display_(/*17.97*/controller),format.raw/*17.107*/(""" """),format.raw/*17.108*/("""= new """),_display_(/*17.115*/{packageName.map(_ + ".").getOrElse("")}),format.raw/*17.155*/("""javascript.Reverse"""),_display_(/*17.174*/(controller)),format.raw/*17.186*/("""(RoutesPrefix.byNamePrefix());""")))}),format.raw/*17.217*/("""
  """),_display_(/*18.4*/cb),format.raw/*18.6*/("""
""")))} else {null} ),format.raw/*19.2*/("""
"""),_display_(/*20.2*/cb),format.raw/*20.4*/("""
"""))
      }
    }
  }

  def render(sourceInfo:RoutesSourceInfo,pkg:Option[String],packageName:Option[String],controllers:Seq[String],jsReverseRouter:Boolean): play.routes.compiler.ScalaFormat.Appendable = apply(sourceInfo,pkg,packageName,controllers,jsReverseRouter)

  def f:((RoutesSourceInfo,Option[String],Option[String],Seq[String],Boolean) => play.routes.compiler.ScalaFormat.Appendable) = (sourceInfo,pkg,packageName,controllers,jsReverseRouter) => apply(sourceInfo,pkg,packageName,controllers,jsReverseRouter)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/static/javaWrappers.scala.twirl
                  HASH: 9e5bb9e12f81767633248b86e50ca93431b49c62
                  MATRIX: 292->1|329->32|768->73|1001->206|1074->255|1092->265|1119->272|1147->275|1220->328|1248->330|1281->338|1328->365|1392->402|1415->404|1445->408|1491->438|1531->440|1561->443|1609->464|1670->504|1705->512|1736->522|1765->524|1796->534|1825->535|1860->542|1922->582|1958->590|1992->602|2055->633|2103->654|2143->656|2173->659|2232->691|2255->693|2287->699|2333->729|2373->731|2405->736|2453->757|2514->797|2560->816|2591->826|2620->828|2652->838|2682->839|2717->846|2779->886|2826->905|2860->917|2923->948|2953->952|2975->954|3020->956|3048->958|3070->960
                  LINES: 10->1|11->2|16->3|21->4|22->5|22->5|22->5|24->7|24->7|26->9|26->9|26->9|28->11|28->11|29->12|29->12|29->12|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|30->13|31->14|31->14|32->15|32->15|32->15|33->16|33->16|33->16|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|35->18|35->18|36->19|37->20|37->20
                  -- GENERATED --
              */
          