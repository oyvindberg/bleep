
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object textarea extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generate an HTML textarea.
 *
 * Example:
 * {{{
 * @textarea(field = myForm("address"), args = Symbol("rows") -> 3, Symbol("cols") -> 50)
 * }}}
 *
 * @param field The form field.
 * @param args Set of extra attributes.
 * @param handler The field constructor.
 */
  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](_display_(/*14.2*/input(field, args:_*)/*14.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*14.56*/("""
    """),format.raw/*15.5*/("""<textarea id=""""),_display_(/*15.20*/id),format.raw/*15.22*/("""" name=""""),_display_(/*15.31*/name),format.raw/*15.35*/("""" """),_display_(/*15.38*/toHtmlArgs(htmlArgs)),format.raw/*15.58*/(""">"""),_display_(/*15.60*/value),format.raw/*15.65*/("""</textarea>
""")))}),format.raw/*16.2*/("""
"""))
      }
    }
  }

  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(handler,messages)

  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(handler,messages)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/textarea.scala.html
                  HASH: 26326b31736e61a475599435ab28a885c47a7ee8
                  MATRIX: 964->274|1186->403|1216->424|1287->457|1319->462|1361->477|1384->479|1420->488|1445->492|1475->495|1516->515|1545->517|1571->522|1614->535
                  LINES: 26->13|31->14|31->14|31->14|32->15|32->15|32->15|32->15|32->15|32->15|32->15|32->15|32->15|33->16
                  -- GENERATED --
              */
          