
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object style extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[Array[(Symbol,String)],Html,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
* Generate an inline style with CSP nonce.
*
* Example:
* {{{
* @style(args = Symbol("type") -> "text/css") {
*   ...
* }
* }}}
*
* See <a href="https://www.w3.org/TR/html51/document-metadata.html#elementdef-style">style element</a>
* for more details.
*
* @param args Set of extra HTML attributes.
* @param body The style body.
*/
  def apply/*17.2*/(args: (Symbol,String)*)(body: => Html)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*18.1*/("""
"""),format.raw/*19.1*/("""<style """),_display_(/*19.9*/{CSPNonce.attr}),format.raw/*19.24*/(""" """),_display_(/*19.26*/toHtmlArgs(args.toMap)),format.raw/*19.48*/(""">"""),_display_(/*19.50*/body),format.raw/*19.54*/("""</style>"""))
      }
    }
  }

  def render(args:Array[(Symbol,String)],body:Html,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(args.toIndexedSeq: _*)(body)(request)

  def f:((Array[(Symbol,String)]) => (=> Html) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (args) => (body) => (request) => apply(args.toIndexedSeq: _*)(body)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/style.scala.html
                  HASH: 201ee57d526ee88230703138150c45fcc2ab6741
                  MATRIX: 991->337|1171->423|1199->424|1233->432|1269->447|1298->449|1341->471|1370->473|1395->477
                  LINES: 30->17|35->18|36->19|36->19|36->19|36->19|36->19|36->19|36->19
                  -- GENERATED --
              */
          