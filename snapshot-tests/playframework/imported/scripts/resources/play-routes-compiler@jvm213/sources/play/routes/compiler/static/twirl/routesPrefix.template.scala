
package play.routes.compiler.static.twirl

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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
                  HASH: b27eb08a365e9195b7eada456bc93507ea8422c3
                  MATRIX: 292->1|329->32|750->73|933->156|1006->205|1024->215|1051->222|1080->304|1107->305|1142->314|1153->317|1195->339|1223->341|1245->343|1274->346|1321->367|1343->369|1375->374|1474->446|1497->448|1531->455|1574->472|1596->474|1628->479|1727->551|1750->553|1779->554|1820->568|1843->570|1873->574|1895->576|1923->578|1945->580
                  LINES: 10->1|11->2|16->3|21->4|22->5|22->5|22->5|24->7|25->8|25->8|25->8|25->8|25->8|25->8|26->9|26->9|26->9|27->10|28->11|28->11|29->12|30->13|30->13|31->14|32->15|32->15|32->15|32->15|32->15|33->16|33->16|34->17|34->17
                  -- GENERATED --
              */
          