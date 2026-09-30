
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object jsloader extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /* TODO: Remove the dependency to jQuery? */
  def apply/*7.2*/()(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*8.1*/("""
"""),_display_(/*9.2*/script(Symbol("type") -> "text/javascript")/*9.45*/ {_display_(Seq[Any](format.raw/*9.47*/("""
"""),format.raw/*10.1*/("""var require = function(moduleName) """),format.raw/*10.36*/("""{"""),format.raw/*10.37*/("""
  """),format.raw/*11.3*/("""var body = "";
  $.ajax("""),format.raw/*12.10*/("""{"""),format.raw/*12.11*/("""
    """),format.raw/*13.5*/("""url: "/assets/javascripts/" + moduleName + ".js",
    dataType: "text", async: false,
    success: function(result) """),format.raw/*15.31*/("""{"""),format.raw/*15.32*/(""" """),format.raw/*15.33*/("""body = result; """),format.raw/*15.48*/("""}"""),format.raw/*15.49*/("""
  """),format.raw/*16.3*/("""}"""),format.raw/*16.4*/(""");
  body = "var exports = """),format.raw/*17.25*/("""{"""),format.raw/*17.26*/("""}"""),format.raw/*17.27*/(""";\n" + body + "\nreturn exports;";
  var fnct = new Function("module", "exports", body);
  return fnct();
"""),format.raw/*20.1*/("""}"""),format.raw/*20.2*/("""
""")))}))
      }
    }
  }

  def render(request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply()(request)

  def f:(() => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = () => (request) => apply()(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/jsloader.scala.html
                  HASH: 8d1426f98e0ec140d1cf58953e4b11c55078f8a2
                  MATRIX: 712->188|854->237|881->239|932->282|971->284|999->285|1062->320|1091->321|1121->324|1173->348|1202->349|1234->354|1378->470|1407->471|1436->472|1479->487|1508->488|1538->491|1566->492|1621->519|1650->520|1679->521|1812->627|1840->628
                  LINES: 16->7|21->8|22->9|22->9|22->9|23->10|23->10|23->10|24->11|25->12|25->12|26->13|28->15|28->15|28->15|28->15|28->15|29->16|29->16|30->17|30->17|30->17|33->20|33->20
                  -- GENERATED --
              */
          