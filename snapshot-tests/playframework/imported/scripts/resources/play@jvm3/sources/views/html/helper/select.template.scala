
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object select extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generate an HTML select.
 *
 * Example:
 * {{{
 * @select(
 *   field = myForm("mySelect"),
 *   options = Seq(
 *     "Foo" -> "foo text",
 *     "Bar" -> "bar text",
 *     "Baz" -> "baz text"
 *    ),
 *   Symbol("_default") -> "Choose One",
 *   Symbol("_disabled") -> Seq("FooKey", "BazKey")
 *   Symbol("cust_att_name") -> "cust_att_value"
 * )
 * }}}
 *
 * @param field The form field.
 * @param options Sequence of options as pairs of value and HTML.
 * @param args Set of extra attributes.
 * @param handler The field constructor.
 */
  def apply/*24.2*/(field: play.api.data.Field, options: Seq[(String,String)], args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](_display_(/*25.2*/input(field, args:_*)/*25.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*25.56*/("""
    """),_display_(/*26.6*/defining( if( htmlArgs.contains(Symbol("multiple")) ) "%s[]".format(name) else name )/*26.91*/ { selectName =>_display_(Seq[Any](format.raw/*26.107*/("""
    """),_display_(/*27.6*/defining( field.indexes.nonEmpty && htmlArgs.contains(Symbol("multiple")) match {
            case true => field.indexes.map( i => field("[%s]".format(i)).value ).flatten.toSet
            case _ => field.value.toSet
    })/*30.7*/{ selectedValues =>_display_(Seq[Any](format.raw/*30.26*/("""
        """),format.raw/*31.9*/("""<select id=""""),_display_(/*31.22*/id),format.raw/*31.24*/("""" name=""""),_display_(/*31.33*/selectName),format.raw/*31.43*/("""" """),_display_(/*31.46*/toHtmlArgs(htmlArgs)),format.raw/*31.66*/(""">
            """),_display_(/*32.14*/args/*32.18*/.toMap.get(Symbol("_default")).map/*32.52*/ { defaultValue =>_display_(Seq[Any](format.raw/*32.70*/("""
                """),format.raw/*33.17*/("""<option class="blank" value="">"""),_display_(/*33.49*/translate(defaultValue)),format.raw/*33.72*/("""</option>
            """)))}),format.raw/*34.14*/("""
            """),_display_(/*35.14*/options/*35.21*/.map/*35.25*/ { case (k, v) =>_display_(Seq[Any](format.raw/*35.42*/("""
                """),_display_(/*36.18*/defining( selectedValues.contains(k) )/*36.56*/ { selected =>_display_(Seq[Any](format.raw/*36.70*/("""
                """),_display_(/*37.18*/defining( args.toMap.get(Symbol("_disabled")).exists { case s: Seq[_] => s.asInstanceOf[Seq[String]].contains(k) })/*37.133*/{ disabled =>_display_(Seq[Any](format.raw/*37.146*/("""
                """),format.raw/*38.17*/("""<option value=""""),_display_(/*38.33*/k),format.raw/*38.34*/("""""""),_display_(if(selected)/*38.48*/{_display_(Seq[Any](format.raw/*38.49*/(""" """),format.raw/*38.50*/("""selected="selected"""")))} else {null} ),_display_(if(disabled)/*38.83*/{_display_(Seq[Any](format.raw/*38.84*/(""" """),format.raw/*38.85*/("""disabled""")))} else {null} ),format.raw/*38.94*/(""">"""),_display_(/*38.96*/v),format.raw/*38.97*/("""</option>
            """)))})))})))}),format.raw/*39.16*/("""
        """),format.raw/*40.9*/("""</select>
    """)))})))}),format.raw/*41.7*/("""
""")))}),format.raw/*42.2*/("""
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
                  SOURCE: core/play/src/main/scala/views/helper/select.scala.html
                  HASH: 521c401986b87e9e705c0a74690bbee0e80056dc
                  MATRIX: 1299->552|1552->712|1582->733|1653->766|1685->772|1779->857|1834->873|1866->879|2097->1102|2154->1121|2190->1130|2230->1143|2253->1145|2289->1154|2320->1164|2350->1167|2391->1187|2433->1202|2446->1206|2489->1240|2545->1258|2590->1275|2649->1307|2693->1330|2747->1353|2788->1367|2804->1374|2817->1378|2872->1395|2917->1413|2964->1451|3016->1465|3061->1483|3186->1598|3238->1611|3283->1628|3326->1644|3348->1645|3389->1659|3428->1660|3457->1661|3533->1694|3572->1695|3601->1696|3654->1705|3683->1707|3705->1708|3767->1733|3803->1742|3852->1758|3884->1760
                  LINES: 38->24|43->25|43->25|43->25|44->26|44->26|44->26|45->27|48->30|48->30|49->31|49->31|49->31|49->31|49->31|49->31|49->31|50->32|50->32|50->32|50->32|51->33|51->33|51->33|52->34|53->35|53->35|53->35|53->35|54->36|54->36|54->36|55->37|55->37|55->37|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|57->39|58->40|59->41|60->42
                  -- GENERATED --
              */
          