
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object inputCheckboxGroup extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
* Generate an HTML checkbox group
*
* Example:
* {{{
* @inputCheckboxGroup(
*           contactForm("hobbies"),
*           options = Seq("S" -> "Surfing", "R" -> "Running", "B" -> "Biking","P" -> "Paddling"),
*           Symbol("_label") -> "Hobbies",
*           Symbol("_error") -> contactForm("hobbies").error.map(_.withMessage("select one or more hobbies")))
*
* }}}
*
* @param field The form field.
* @param options Sequence of options as pairs of value and HTML
* @param args Set of extra HTML attributes.
* @param handler The field constructor.
*/
  def apply/*19.2*/(field: play.api.data.Field, options: Seq[(String,String)], args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](_display_(/*20.2*/input(field, args.map{ x => if(x._1 == Symbol("_label")) Symbol("_name") -> x._2 else x }:_*)/*20.95*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*20.128*/("""
  """),format.raw/*21.3*/("""<span class="buttonset" id=""""),_display_(/*21.32*/id),format.raw/*21.34*/("""">
    """),_display_(/*22.6*/defining(field.indexes.map( i => field("[%s]".format(i)).value ).flatten.toSet)/*22.85*/ { values =>_display_(Seq[Any](format.raw/*22.97*/("""
      """),_display_(/*23.8*/options/*23.15*/.map/*23.19*/ { v =>_display_(Seq[Any](format.raw/*23.26*/("""
        """),format.raw/*24.9*/("""<input type="checkbox" id=""""),_display_(/*24.37*/(id)),format.raw/*24.41*/("""_"""),_display_(/*24.43*/v/*24.44*/._1),format.raw/*24.47*/("""" name=""""),_display_(/*24.56*/{name + "[]"}),format.raw/*24.69*/("""" value=""""),_display_(/*24.79*/v/*24.80*/._1),format.raw/*24.83*/("""" """),_display_(if(values.contains(v._1))/*24.111*/{_display_(Seq[Any](format.raw/*24.112*/("""checked="checked"""")))} else {null} ),format.raw/*24.130*/(""" """),_display_(/*24.132*/toHtmlArgs(htmlArgs)),format.raw/*24.152*/("""/>
        <label for=""""),_display_(/*25.22*/(id)),format.raw/*25.26*/("""_"""),_display_(/*25.28*/v/*25.29*/._1),format.raw/*25.32*/("""">"""),_display_(/*25.35*/v/*25.36*/._2),format.raw/*25.39*/("""</label>
      """)))}),format.raw/*26.8*/("""
    """)))}),format.raw/*27.6*/("""
  """),format.raw/*28.3*/("""</span>
""")))}),format.raw/*29.2*/("""
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
                  SOURCE: core/play/src/main/scala/views/helper/inputCheckboxGroup.scala.html
                  HASH: 7eb73d92914646458b05994b980556f03070739d
                  MATRIX: 1320->561|1573->721|1675->814|1747->847|1777->850|1833->879|1856->881|1890->889|1978->968|2028->980|2062->988|2078->995|2091->999|2136->1006|2172->1015|2227->1043|2252->1047|2281->1049|2291->1050|2315->1053|2351->1062|2385->1075|2422->1085|2432->1086|2456->1089|2512->1117|2552->1118|2615->1136|2645->1138|2687->1158|2738->1182|2763->1186|2792->1188|2802->1189|2826->1192|2856->1195|2866->1196|2890->1199|2936->1215|2972->1221|3002->1224|3041->1233
                  LINES: 33->19|38->20|38->20|38->20|39->21|39->21|39->21|40->22|40->22|40->22|41->23|41->23|41->23|41->23|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|43->25|43->25|43->25|43->25|43->25|43->25|43->25|43->25|44->26|45->27|46->28|47->29
                  -- GENERATED --
              */
          