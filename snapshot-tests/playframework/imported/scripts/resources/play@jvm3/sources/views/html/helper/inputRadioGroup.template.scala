
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object inputRadioGroup extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generate an HTML radio group
 *
 * Example:
 * {{{
 * @inputRadioGroup(
 *           contactForm("gender"),
 *           options = Seq("M"->"Male","F"->"Female"),
 *           Symbol("_label") -> "Gender",
 *           Symbol("_error") -> contactForm("gender").error.map(_.withMessage("select gender")))
 *
 * }}}
 *
 * @param field The form field.
 * @param options Seq of radio buttons encoded as value -> label
 * @param args Set of extra HTML attributes.
 * @param handler The field constructor.
 */
  def apply/*19.2*/(field: play.api.data.Field, options: Seq[(String,String)], args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](_display_(/*20.2*/input(field, args.map{ x => if(x._1 == Symbol("_label")) Symbol("_name") -> x._2 else x }:_*)/*20.95*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*20.128*/("""
  """),format.raw/*21.3*/("""<span class="buttonset" id=""""),_display_(/*21.32*/id),format.raw/*21.34*/("""">
    """),_display_(/*22.6*/options/*22.13*/.map/*22.17*/ { v =>_display_(Seq[Any](format.raw/*22.24*/("""
      """),format.raw/*23.7*/("""<input type="radio" id=""""),_display_(/*23.32*/(id)),format.raw/*23.36*/("""_"""),_display_(/*23.38*/v/*23.39*/._1),format.raw/*23.42*/("""" name=""""),_display_(/*23.51*/name),format.raw/*23.55*/("""" value=""""),_display_(/*23.65*/v/*23.66*/._1),format.raw/*23.69*/("""" """),_display_(if(value == Some(v._1))/*23.95*/{_display_(Seq[Any](format.raw/*23.96*/("""checked="checked"""")))} else {null} ),format.raw/*23.114*/(""" """),_display_(/*23.116*/toHtmlArgs(htmlArgs)),format.raw/*23.136*/("""/>
      <label for=""""),_display_(/*24.20*/(id)),format.raw/*24.24*/("""_"""),_display_(/*24.26*/v/*24.27*/._1),format.raw/*24.30*/("""">"""),_display_(/*24.33*/v/*24.34*/._2),format.raw/*24.37*/("""</label>
    """)))}),format.raw/*25.6*/("""
  """),format.raw/*26.3*/("""</span>
""")))}),format.raw/*27.2*/("""
"""))
      }
    }
  }

  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq*)(handler,messages)

  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq*)(handler,messages)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/inputRadioGroup.scala.html
                  HASH: 32777da109601cfe3e205213f9d5d5494c119135
                  MATRIX: 1268->512|1521->672|1623->765|1695->798|1725->801|1781->830|1804->832|1838->840|1854->847|1867->851|1912->858|1946->865|1998->890|2023->894|2052->896|2062->897|2086->900|2122->909|2147->913|2184->923|2194->924|2218->927|2271->953|2310->954|2373->972|2403->974|2445->994|2494->1016|2519->1020|2548->1022|2558->1023|2582->1026|2612->1029|2622->1030|2646->1033|2690->1047|2720->1050|2759->1059
                  LINES: 33->19|38->20|38->20|38->20|39->21|39->21|39->21|40->22|40->22|40->22|40->22|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|43->25|44->26|45->27
                  -- GENERATED --
              */
          