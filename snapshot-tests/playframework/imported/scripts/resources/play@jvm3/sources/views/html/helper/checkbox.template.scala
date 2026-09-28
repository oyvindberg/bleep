
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object checkbox extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generate an HTML input checkbox.
 *
 * Example:
 * {{{
 * @checkbox(field = myForm("done"))
 * }}}
 *
 * @param field The form field.
 * @param args Set of extra HTML attributes ('''id''' and '''label''' are 2 special arguments).
 * @param handler The field constructor.
 * @param messages the provider of messages
 */
  def apply/*14.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {

def /*16.2*/boxValue/*16.10*/ = {{ args.toMap.get(Symbol("value")).getOrElse("true") }};
Seq[Any](format.raw/*15.1*/("""
"""),format.raw/*16.67*/("""
"""),_display_(/*17.2*/input(field, args:_*)/*17.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*17.56*/("""
    """),format.raw/*18.5*/("""<input type="checkbox" id=""""),_display_(/*18.33*/id),format.raw/*18.35*/("""" name=""""),_display_(/*18.44*/name),format.raw/*18.48*/("""" value=""""),_display_(/*18.58*/boxValue),format.raw/*18.66*/("""" """),_display_(if(value == Some(boxValue))/*18.96*/{_display_(Seq[Any](format.raw/*18.97*/("""checked="checked"""")))} else {null} ),format.raw/*18.115*/(""" """),_display_(/*18.117*/toHtmlArgs(htmlArgs.view.filterKeys(_ != Symbol("value")).toMap)),format.raw/*18.181*/("""/>
    <span>"""),_display_(/*19.12*/translate(args.toMap.get(Symbol("_text")))),format.raw/*19.54*/("""</span>
""")))}),format.raw/*20.2*/("""
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
                  SOURCE: core/play/src/main/scala/views/helper/checkbox.scala.html
                  HASH: c743181eae7f035a8249c649eede8c52cc53029e
                  MATRIX: 1055->327|1261->457|1278->465|1365->455|1394->522|1422->524|1452->545|1523->578|1555->583|1610->611|1633->613|1669->622|1694->626|1731->636|1760->644|1817->674|1856->675|1919->693|1949->695|2035->759|2076->773|2139->815|2178->824
                  LINES: 28->14|32->16|32->16|33->15|34->16|35->17|35->17|35->17|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|37->19|37->19|38->20
                  -- GENERATED --
              */
          