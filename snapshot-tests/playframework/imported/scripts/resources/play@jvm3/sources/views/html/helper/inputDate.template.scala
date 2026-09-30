
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object inputDate extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generate an HTML5 input date.
 *
 * Example:
 * {{{
 * @inputDate(field = myForm("releaseDate"), args = Symbol("size") -> 10)
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
    """),format.raw/*15.5*/("""<input type="date" id=""""),_display_(/*15.29*/id),format.raw/*15.31*/("""" name=""""),_display_(/*15.40*/name),format.raw/*15.44*/("""" value=""""),_display_(/*15.54*/value),format.raw/*15.59*/("""" """),_display_(/*15.62*/toHtmlArgs(htmlArgs)),format.raw/*15.82*/("""/>
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
                  SOURCE: core/play/src/main/scala/views/helper/inputDate.scala.html
                  HASH: 8a45f2140d63a6e0ff827cb78030311ace6587ba
                  MATRIX: 990->261|1212->390|1242->411|1313->444|1345->449|1396->473|1419->475|1455->484|1480->488|1517->498|1543->503|1573->506|1614->526|1648->530
                  LINES: 27->13|32->14|32->14|32->14|33->15|33->15|33->15|33->15|33->15|33->15|33->15|33->15|33->15|34->16
                  -- GENERATED --
              */
          