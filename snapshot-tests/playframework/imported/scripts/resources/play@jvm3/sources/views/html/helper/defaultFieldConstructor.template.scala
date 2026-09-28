
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object defaultFieldConstructor extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[FieldElements,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Default field constructor.
 *
 * It generates field as following:
 * {{{
 * <dl class="error">
 *   <dt><label for="name">Your name:</label></dt>
 *   <dd><input type="text" id="name" name="name"></dd>
 *   <dd class="error">This field is required</dd>
 *   <dd class="info">Required</dd>
 * </dl>
 * }}}
 *
 * @param el The field informations.
 */
  def apply/*16.2*/(elements: FieldElements):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*17.1*/("""<dl class=""""),_display_(/*17.13*/elements/*17.21*/.args.get(Symbol("_class"))),format.raw/*17.48*/(""" """),_display_(if(elements.hasErrors)/*17.72*/ {_display_(Seq[Any](format.raw/*17.74*/("""error""")))} else {null} ),format.raw/*17.80*/("""" id=""""),_display_(/*17.87*/elements/*17.95*/.args.get(Symbol("_id")).getOrElse(elements.id + "_field")),format.raw/*17.153*/("""">
    """),_display_(if(elements.hasName)/*18.26*/ {_display_(Seq[Any](format.raw/*18.28*/("""
    """),format.raw/*19.5*/("""<dt>"""),_display_(/*19.10*/elements/*19.18*/.name),format.raw/*19.23*/("""</dt>
    """)))}else/*20.12*/{_display_(Seq[Any](format.raw/*20.13*/("""
    """),format.raw/*21.5*/("""<dt><label for=""""),_display_(/*21.22*/elements/*21.30*/.id),format.raw/*21.33*/("""">"""),_display_(/*21.36*/elements/*21.44*/.label),format.raw/*21.50*/("""</label></dt>
    """)))}),format.raw/*22.6*/("""
    """),format.raw/*23.5*/("""<dd>"""),_display_(/*23.10*/elements/*23.18*/.input),format.raw/*23.24*/("""</dd>
    """),_display_(/*24.6*/elements/*24.14*/.errors.map/*24.25*/ { error =>_display_(Seq[Any](format.raw/*24.36*/("""
        """),format.raw/*25.9*/("""<dd class="error">"""),_display_(/*25.28*/error),format.raw/*25.33*/("""</dd>
    """)))}),format.raw/*26.6*/("""
    """),_display_(/*27.6*/elements/*27.14*/.infos.map/*27.24*/ { info =>_display_(Seq[Any](format.raw/*27.34*/("""
        """),format.raw/*28.9*/("""<dd class="info">"""),_display_(/*28.27*/info),format.raw/*28.31*/("""</dd>
    """)))}),format.raw/*29.6*/("""
"""),format.raw/*30.1*/("""</dl>
"""))
      }
    }
  }

  def render(elements:FieldElements): play.twirl.api.HtmlFormat.Appendable = apply(elements)

  def f:((FieldElements) => play.twirl.api.HtmlFormat.Appendable) = (elements) => apply(elements)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/defaultFieldConstructor.scala.html
                  HASH: a36560039c31752a52beaf49419779d084b4d440
                  MATRIX: 1026->357|1146->383|1185->395|1202->403|1250->430|1301->454|1341->456|1391->462|1425->469|1442->477|1522->535|1577->563|1617->565|1649->570|1681->575|1698->583|1724->588|1758->605|1797->606|1829->611|1873->628|1890->636|1914->639|1944->642|1961->650|1988->656|2037->675|2069->680|2101->685|2118->693|2145->699|2182->710|2199->718|2219->729|2268->740|2304->749|2350->768|2376->773|2417->784|2449->790|2466->798|2485->808|2533->818|2569->827|2614->845|2639->849|2680->860|2708->861
                  LINES: 30->16|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|36->18|36->18|37->19|37->19|37->19|37->19|38->20|38->20|39->21|39->21|39->21|39->21|39->21|39->21|39->21|40->22|41->23|41->23|41->23|41->23|42->24|42->24|42->24|42->24|43->25|43->25|43->25|44->26|45->27|45->27|45->27|45->27|46->28|46->28|46->28|47->29|48->30
                  -- GENERATED --
              */
          