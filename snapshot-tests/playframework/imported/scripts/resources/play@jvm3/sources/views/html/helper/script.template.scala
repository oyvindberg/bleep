
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object script extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[Array[(Symbol,String)],Html,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
* Generate an inline script with CSP nonce.
*
* Example:
* {{{
* @script(args = Symbol("type") -> "text/javascript") {
*   ...
* }
* }}}
*
* See <a href="https://www.w3.org/TR/2016/REC-html51-20161101/semantics-scripting.html">Scripting</a>
* for more information.
*
* @param args Set of extra HTML attributes.
* @param body The script body.
*/
  def apply/*17.2*/(args: (Symbol,String)*)(body: => Html)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*18.1*/("""
"""),format.raw/*19.1*/("""<script """),_display_(/*19.10*/{CSPNonce.attr}),format.raw/*19.25*/(""" """),_display_(/*19.27*/toHtmlArgs(args.toMap)),format.raw/*19.49*/(""">"""),_display_(/*19.51*/body),format.raw/*19.55*/("""</script>"""))
      }
    }
  }

  def render(args:Array[(Symbol,String)],body:Html,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(args.toIndexedSeq*)(body)(request)

  def f:((Array[(Symbol,String)]) => (=> Html) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (args) => (body) => (request) => apply(args.toIndexedSeq*)(body)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/script.scala.html
                  HASH: 6236c233b0ab7d8f821608cddd082020a50419d2
                  MATRIX: 1043->350|1223->436|1251->437|1287->446|1323->461|1352->463|1395->485|1424->487|1449->491
                  LINES: 31->17|36->18|37->19|37->19|37->19|37->19|37->19|37->19|37->19
                  -- GENERATED --
              */
          