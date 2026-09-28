
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object input extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Array[(Symbol, Any)],(String, String, Option[String], Map[Symbol,Any]) => Html,FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Prepare a generic HTML input.
 */
  def apply/*4.2*/(field: play.api.data.Field, args: (Symbol, Any)* )(inputDef: (String, String, Option[String], Map[Symbol,Any]) => Html)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {

def /*5.2*/id/*5.4*/ = {{ args.toMap.get(Symbol("id")).map(_.toString).getOrElse(field.id) }};
Seq[Any](format.raw/*5.76*/("""
"""),_display_(/*6.2*/handler(
    FieldElements(
        id,
        field,
        inputDef(id, field.name, field.value, args.filter(arg => !arg._1.name.startsWith("_") && arg._1 != Symbol("id")).toMap),
        args.toMap,
        messages
    )
)),format.raw/*14.2*/("""
"""))
      }
    }
  }

  def render(field:play.api.data.Field,args:Array[(Symbol, Any)],inputDef:(String, String, Option[String], Map[Symbol,Any]) => Html,handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(inputDef)(handler,messages)

  def f:((play.api.data.Field,Array[(Symbol, Any)]) => ((String, String, Option[String], Map[Symbol,Any]) => Html) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (inputDef) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(inputDef)(handler,messages)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/input.scala.html
                  HASH: 808247a3df7f25487ad378583f5bbbbecb16024e
                  MATRIX: 787->42|1063->242|1072->244|1174->316|1201->318|1449->546
                  LINES: 17->4|21->5|21->5|22->5|23->6|31->14
                  -- GENERATED --
              */
          