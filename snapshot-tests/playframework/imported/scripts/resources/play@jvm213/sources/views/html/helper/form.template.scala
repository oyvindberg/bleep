
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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

  def render(action:play.api.mvc.Call,args:Array[(Symbol,String)],body:Html): play.twirl.api.HtmlFormat.Appendable = apply(action,args.toIndexedSeq: _*)(body)

  def f:((play.api.mvc.Call,Array[(Symbol,String)]) => (=> Html) => play.twirl.api.HtmlFormat.Appendable) = (action,args) => (body) => apply(action,args.toIndexedSeq: _*)(body)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/form.scala.html
                  HASH: 36f116b71abe2cfe38737fec8a9f3599e503e526
                  MATRIX: 913->269|1075->335|1104->337|1146->352|1161->358|1187->363|1225->374|1240->380|1268->387|1298->390|1341->412|1374->419|1399->423|1427->424
                  LINES: 28->15|33->15|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|35->17|35->17|36->18
                  -- GENERATED --
              */
          