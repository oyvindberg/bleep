
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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

  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq: _*)(handler,messages)

  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq: _*)(handler,messages)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/inputRadioGroup.scala.html
                  HASH: b9a825e67f7f15d7d26dfd3727682d1d5b2d5366
                  MATRIX: 1230->512|1483->672|1585->765|1657->798|1687->801|1743->830|1766->832|1800->840|1816->847|1829->851|1874->858|1908->865|1960->890|1985->894|2014->896|2024->897|2048->900|2084->909|2109->913|2146->923|2156->924|2180->927|2233->953|2272->954|2335->972|2365->974|2407->994|2456->1016|2481->1020|2510->1022|2520->1023|2544->1026|2574->1029|2584->1030|2608->1033|2652->1047|2682->1050|2721->1059
                  LINES: 32->19|37->20|37->20|37->20|38->21|38->21|38->21|39->22|39->22|39->22|39->22|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|42->25|43->26|44->27
                  -- GENERATED --
              */
          