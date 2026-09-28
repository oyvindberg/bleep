
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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
                  HASH: 429e6678ce203edf83d5fa63bd29e128797c3661
                  MATRIX: 988->357|1108->383|1147->395|1164->403|1212->430|1263->454|1303->456|1353->462|1387->469|1404->477|1484->535|1539->563|1579->565|1611->570|1643->575|1660->583|1686->588|1720->605|1759->606|1791->611|1835->628|1852->636|1876->639|1906->642|1923->650|1950->656|1999->675|2031->680|2063->685|2080->693|2107->699|2144->710|2161->718|2181->729|2230->740|2266->749|2312->768|2338->773|2379->784|2411->790|2428->798|2447->808|2495->818|2531->827|2576->845|2601->849|2642->860|2670->861
                  LINES: 29->16|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|35->18|35->18|36->19|36->19|36->19|36->19|37->20|37->20|38->21|38->21|38->21|38->21|38->21|38->21|38->21|39->22|40->23|40->23|40->23|40->23|41->24|41->24|41->24|41->24|42->25|42->25|42->25|43->26|44->27|44->27|44->27|44->27|45->28|45->28|45->28|46->29|47->30
                  -- GENERATED --
              */
          