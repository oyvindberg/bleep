
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

object routesPrefix extends _root_.play.twirl.api.BaseScalaTemplate[play.routes.compiler.ScalaFormat.Appendable,_root_.play.twirl.api.Format[play.routes.compiler.ScalaFormat.Appendable]](play.routes.compiler.ScalaFormat) with _root_.play.twirl.api.Template3[RoutesSourceInfo,Option[String],Route => Boolean,play.routes.compiler.ScalaFormat.Appendable] {

  /**/
  def apply/*3.2*/(sourceInfo: RoutesSourceInfo, pkg: Option[String], useInjector: Route => Boolean):play.routes.compiler.ScalaFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*4.1*/("""// @GENERATOR:play-routes-compiler
// @SOURCE:"""),_display_(/*5.14*/sourceInfo/*5.24*/.source),format.raw/*5.31*/("""

"""),format.raw/*7.81*/("""
"""),format.raw/*8.1*/("""package """),_display_(/*8.10*/pkg/*8.13*/.getOrElse("_routes_")),format.raw/*8.35*/(""" """),_display_(/*8.37*/ob),format.raw/*8.39*/("""
  """),format.raw/*9.3*/("""object RoutesPrefix """),_display_(/*9.24*/ob),format.raw/*9.26*/("""
    """),format.raw/*10.5*/("""private var _prefix: String = "/"
    def setPrefix(p: String): Unit = """),_display_(/*11.39*/ob),format.raw/*11.41*/("""
      """),format.raw/*12.7*/("""_prefix = p
    """),_display_(/*13.6*/cb),format.raw/*13.8*/("""
    """),format.raw/*14.5*/("""def prefix: String = _prefix
    val byNamePrefix: Function0[String] = """),_display_(/*15.44*/ob),format.raw/*15.46*/(""" """),format.raw/*15.47*/("""() => prefix """),_display_(/*15.61*/cb),format.raw/*15.63*/("""
  """),_display_(/*16.4*/cb),format.raw/*16.6*/("""
"""),_display_(/*17.2*/cb),format.raw/*17.4*/("""
"""))
      }
    }
  }

  def render(sourceInfo:RoutesSourceInfo,pkg:Option[String],useInjector:Route => Boolean): play.routes.compiler.ScalaFormat.Appendable = apply(sourceInfo,pkg,useInjector)

  def f:((RoutesSourceInfo,Option[String],Route => Boolean) => play.routes.compiler.ScalaFormat.Appendable) = (sourceInfo,pkg,useInjector) => apply(sourceInfo,pkg,useInjector)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: dev-mode/play-routes-compiler/src/main/twirl/play/routes/compiler/static/routesPrefix.scala.twirl
                  HASH: 37d9e462038f5a02e0be325ec659dcc169cbbefa
                  MATRIX: 330->1|367->32|788->73|971->156|1044->205|1062->215|1089->222|1118->304|1145->305|1180->314|1191->317|1233->339|1261->341|1283->343|1312->346|1359->367|1381->369|1413->374|1512->446|1535->448|1569->455|1612->472|1634->474|1666->479|1765->551|1788->553|1817->554|1858->568|1881->570|1911->574|1933->576|1961->578|1983->580
                  LINES: 11->1|12->2|17->3|22->4|23->5|23->5|23->5|25->7|26->8|26->8|26->8|26->8|26->8|26->8|27->9|27->9|27->9|28->10|29->11|29->11|30->12|31->13|31->13|32->14|33->15|33->15|33->15|33->15|33->15|34->16|34->16|35->17|35->17
                  -- GENERATED --
              */
          