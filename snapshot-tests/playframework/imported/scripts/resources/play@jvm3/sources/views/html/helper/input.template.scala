
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
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

  def render(field:play.api.data.Field,args:Array[(Symbol, Any)],inputDef:(String, String, Option[String], Map[Symbol,Any]) => Html,handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq*)(inputDef)(handler,messages)

  def f:((play.api.data.Field,Array[(Symbol, Any)]) => ((String, String, Option[String], Map[Symbol,Any]) => Html) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (inputDef) => (handler,messages) => apply(field,args.toIndexedSeq*)(inputDef)(handler,messages)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/input.scala.html
                  HASH: 835c4c6479e916e096883b01008931708bcf599e
                  MATRIX: 825->42|1101->242|1110->244|1212->316|1239->318|1487->546
                  LINES: 18->4|22->5|22->5|23->5|24->6|32->14
                  -- GENERATED --
              */
          