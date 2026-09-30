
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object inputPassword extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generate an HTML input password.
 *
 * Example:
 * {{{
 * @inputPassword(field = myForm("password"), args = Symbol("size") -> 10)
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
    """),format.raw/*15.5*/("""<input type="password" id=""""),_display_(/*15.33*/id),format.raw/*15.35*/("""" name=""""),_display_(/*15.44*/name),format.raw/*15.48*/("""" """),_display_(/*15.51*/toHtmlArgs(htmlArgs)),format.raw/*15.71*/("""/>
""")))}),format.raw/*16.2*/("""
"""))
      }
    }
  }

  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq*)(handler,messages)

  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq*)(handler,messages)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/inputPassword.scala.html
                  HASH: 299dafad423b651156810f70807ccb2a55e6a1bf
                  MATRIX: 998->265|1220->394|1250->415|1321->448|1353->453|1408->481|1431->483|1467->492|1492->496|1522->499|1563->519|1597->523
                  LINES: 27->13|32->14|32->14|32->14|33->15|33->15|33->15|33->15|33->15|33->15|33->15|34->16
                  -- GENERATED --
              */
          