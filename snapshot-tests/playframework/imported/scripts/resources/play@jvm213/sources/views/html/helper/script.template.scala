
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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

  def render(args:Array[(Symbol,String)],body:Html,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(args.toIndexedSeq: _*)(body)(request)

  def f:((Array[(Symbol,String)]) => (=> Html) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (args) => (body) => (request) => apply(args.toIndexedSeq: _*)(body)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/script.scala.html
                  HASH: 5d104820328161da5dce8671010304183b6e4b2a
                  MATRIX: 1005->350|1185->436|1213->437|1249->446|1285->461|1314->463|1357->485|1386->487|1411->491
                  LINES: 30->17|35->18|36->19|36->19|36->19|36->19|36->19|36->19|36->19
                  -- GENERATED --
              */
          