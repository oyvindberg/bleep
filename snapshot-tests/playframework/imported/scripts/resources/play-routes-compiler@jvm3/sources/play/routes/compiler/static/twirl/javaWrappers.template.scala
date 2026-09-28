
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
                  HASH: b21696999544dad06f38faaedcd8f7adabda0a97
                  MATRIX: 330->1|367->32|806->73|1039->206|1112->255|1130->265|1157->272|1185->275|1258->328|1286->330|1319->338|1366->365|1430->402|1453->404|1483->408|1529->438|1569->440|1599->443|1647->464|1708->504|1743->512|1774->522|1803->524|1834->534|1863->535|1898->542|1960->582|1996->590|2030->602|2093->633|2141->654|2181->656|2211->659|2270->691|2293->693|2325->699|2371->729|2411->731|2443->736|2491->757|2552->797|2598->816|2629->826|2658->828|2690->838|2720->839|2755->846|2817->886|2864->905|2898->917|2961->948|2991->952|3013->954|3058->956|3086->958|3108->960
                  LINES: 11->1|12->2|17->3|22->4|23->5|23->5|23->5|25->7|25->7|27->9|27->9|27->9|29->11|29->11|30->12|30->12|30->12|31->13|31->13|31->13|31->13|31->13|31->13|31->13|31->13|31->13|31->13|31->13|31->13|31->13|32->14|32->14|33->15|33->15|33->15|34->16|34->16|34->16|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|36->18|36->18|37->19|38->20|38->20
                  -- GENERATED --
              */
          