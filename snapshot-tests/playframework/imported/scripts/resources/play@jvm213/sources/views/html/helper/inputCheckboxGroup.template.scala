
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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

  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq: _*)(handler,messages)

  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq: _*)(handler,messages)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/inputCheckboxGroup.scala.html
                  HASH: 506c64f467048a43060a6c60a8663a3b363a2404
                  MATRIX: 1282->561|1535->721|1637->814|1709->847|1739->850|1795->879|1818->881|1852->889|1940->968|1990->980|2024->988|2040->995|2053->999|2098->1006|2134->1015|2189->1043|2214->1047|2243->1049|2253->1050|2277->1053|2313->1062|2347->1075|2384->1085|2394->1086|2418->1089|2474->1117|2514->1118|2577->1136|2607->1138|2649->1158|2700->1182|2725->1186|2754->1188|2764->1189|2788->1192|2818->1195|2828->1196|2852->1199|2898->1215|2934->1221|2964->1224|3003->1233
                  LINES: 32->19|37->20|37->20|37->20|38->21|38->21|38->21|39->22|39->22|39->22|40->23|40->23|40->23|40->23|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|42->25|42->25|42->25|42->25|42->25|42->25|42->25|42->25|43->26|44->27|45->28|46->29
                  -- GENERATED --
              */
          