
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object inputText extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generate an HTML input text.
 *
 * Example:
 * {{{
 * @inputText(field = myForm("name"), args = Symbol("size") -> 10, Symbol("placeholder") -> "Your name")
 * }}}
 *
 * @param field The form field.
 * @param args Set of extra attributes.
 * @param handler The field constructor.
 */
  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {

def /*14.2*/inputType/*14.11*/ = {{ args.toMap.get(Symbol("type")).map(_.toString).getOrElse("text") }};
Seq[Any](format.raw/*14.83*/("""
"""),_display_(/*15.2*/input(field, args.filter(_._1 != Symbol("type")):_*)/*15.54*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*15.87*/("""
    """),format.raw/*16.5*/("""<input type=""""),_display_(/*16.19*/inputType),format.raw/*16.28*/("""" id=""""),_display_(/*16.35*/id),format.raw/*16.37*/("""" name=""""),_display_(/*16.46*/name),format.raw/*16.50*/("""" value=""""),_display_(/*16.60*/value),format.raw/*16.65*/("""" """),_display_(/*16.68*/toHtmlArgs(htmlArgs)),format.raw/*16.88*/("""/>
""")))}),format.raw/*17.2*/("""
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
                  SOURCE: core/play/src/main/scala/views/helper/inputText.scala.html
                  HASH: 6488ba3edfd2a3ce8ba6b549b8246eecbe8672c3
                  MATRIX: 1020->291|1226->420|1244->429|1347->501|1375->503|1436->555|1507->588|1539->593|1580->607|1610->616|1644->623|1667->625|1703->634|1728->638|1765->648|1791->653|1821->656|1862->676|1896->680
                  LINES: 27->13|31->14|31->14|32->14|33->15|33->15|33->15|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|35->17
                  -- GENERATED --
              */
          