
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object inputFile extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generate an HTML input file.
 *
 * Example:
 * {{{
 * @inputFile(field = myForm("name"), args = Symbol("size") -> 10)
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
    """),format.raw/*15.5*/("""<input type="file" id=""""),_display_(/*15.29*/id),format.raw/*15.31*/("""" name=""""),_display_(/*15.40*/name),format.raw/*15.44*/("""" """),_display_(/*15.47*/toHtmlArgs(htmlArgs)),format.raw/*15.67*/("""/>
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
                  SOURCE: core/play/src/main/scala/views/helper/inputFile.scala.html
                  HASH: e6276cd6bb62a52e41adb3b1fd2ed8ad7a2f6316
                  MATRIX: 944->253|1166->382|1196->403|1267->436|1299->441|1350->465|1373->467|1409->476|1434->480|1464->483|1505->503|1539->507
                  LINES: 26->13|31->14|31->14|31->14|32->15|32->15|32->15|32->15|32->15|32->15|32->15|33->16
                  -- GENERATED --
              */
          