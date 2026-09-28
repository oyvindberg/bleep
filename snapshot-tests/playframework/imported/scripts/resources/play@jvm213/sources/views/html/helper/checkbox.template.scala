
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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

  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(handler,messages)

  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(handler,messages)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/checkbox.scala.html
                  HASH: 581021bd2245c294dd874ef65aa603c6b00bab1d
                  MATRIX: 1017->327|1223->457|1240->465|1327->455|1356->522|1384->524|1414->545|1485->578|1517->583|1572->611|1595->613|1631->622|1656->626|1693->636|1722->644|1779->674|1818->675|1881->693|1911->695|1997->759|2038->773|2101->815|2140->824
                  LINES: 27->14|31->16|31->16|32->15|33->16|34->17|34->17|34->17|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|36->19|36->19|37->20
                  -- GENERATED --
              */
          