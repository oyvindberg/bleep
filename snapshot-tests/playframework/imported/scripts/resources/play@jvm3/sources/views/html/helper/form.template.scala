
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object form extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[play.api.mvc.Call,Array[(Symbol,String)],Html,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generate an HTML form.
 *
 * Example:
 * {{{
 * @form(action = routes.Users.submit, args = Symbol("class") -> "myForm") {
 *   ...
 * }
 * }}}
 *
 * @param action The submit action.
 * @param args Set of extra HTML attributes.
 * @param body The form body.
 */
  def apply/*15.2*/(action: play.api.mvc.Call, args: (Symbol,String)*)(body: => Html):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*15.68*/(""" 
"""),format.raw/*16.1*/("""<form action=""""),_display_(/*16.16*/action/*16.22*/.path),format.raw/*16.27*/("""" method=""""),_display_(/*16.38*/action/*16.44*/.method),format.raw/*16.51*/("""" """),_display_(/*16.54*/toHtmlArgs(args.toMap)),format.raw/*16.76*/(""">
    """),_display_(/*17.6*/body),format.raw/*17.10*/("""
"""),format.raw/*18.1*/("""</form>
"""))
      }
    }
  }

  def render(action:play.api.mvc.Call,args:Array[(Symbol,String)],body:Html): play.twirl.api.HtmlFormat.Appendable = apply(action,args.toIndexedSeq*)(body)

  def f:((play.api.mvc.Call,Array[(Symbol,String)]) => (=> Html) => play.twirl.api.HtmlFormat.Appendable) = (action,args) => (body) => apply(action,args.toIndexedSeq*)(body)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/form.scala.html
                  HASH: f9543e0bd166d58eadff2c1e3f467406deb338c3
                  MATRIX: 951->269|1113->335|1142->337|1184->352|1199->358|1225->363|1263->374|1278->380|1306->387|1336->390|1379->412|1412->419|1437->423|1465->424
                  LINES: 29->15|34->15|35->16|35->16|35->16|35->16|35->16|35->16|35->16|35->16|35->16|36->17|36->17|37->18
                  -- GENERATED --
              */
          