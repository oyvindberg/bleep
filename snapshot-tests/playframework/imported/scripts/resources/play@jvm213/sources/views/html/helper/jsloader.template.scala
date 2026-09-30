
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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
                  HASH: 6c7cd39ab3c5698eda01d1b1a009d36a6c88bf7e
                  MATRIX: 674->188|816->237|843->239|894->282|933->284|961->285|1024->320|1053->321|1083->324|1135->348|1164->349|1196->354|1340->470|1369->471|1398->472|1441->487|1470->488|1500->491|1528->492|1583->519|1612->520|1641->521|1774->627|1802->628
                  LINES: 15->7|20->8|21->9|21->9|21->9|22->10|22->10|22->10|23->11|24->12|24->12|25->13|27->15|27->15|27->15|27->15|27->15|28->16|28->16|29->17|29->17|29->17|32->20|32->20
                  -- GENERATED --
              */
          