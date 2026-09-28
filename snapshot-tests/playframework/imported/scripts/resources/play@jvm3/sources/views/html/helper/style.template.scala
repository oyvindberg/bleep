
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
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

  def render(args:Array[(Symbol,String)],body:Html,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(args.toIndexedSeq*)(body)(request)

  def f:((Array[(Symbol,String)]) => (=> Html) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (args) => (body) => (request) => apply(args.toIndexedSeq*)(body)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/style.scala.html
                  HASH: adb03062cce963c9a2ac3e13ac1a5626a93ab349
                  MATRIX: 1029->337|1209->423|1237->424|1271->432|1307->447|1336->449|1379->471|1408->473|1433->477
                  LINES: 31->17|36->18|37->19|37->19|37->19|37->19|37->19|37->19|37->19
                  -- GENERATED --
              */
          