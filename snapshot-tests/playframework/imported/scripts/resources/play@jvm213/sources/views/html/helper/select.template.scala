
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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

  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq: _*)(handler,messages)

  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq: _*)(handler,messages)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/select.scala.html
                  HASH: cc0d6a16c55701852a1c335224961bd913bd0a80
                  MATRIX: 1261->552|1514->712|1544->733|1615->766|1647->772|1741->857|1796->873|1828->879|2059->1102|2116->1121|2152->1130|2192->1143|2215->1145|2251->1154|2282->1164|2312->1167|2353->1187|2395->1202|2408->1206|2451->1240|2507->1258|2552->1275|2611->1307|2655->1330|2709->1353|2750->1367|2766->1374|2779->1378|2834->1395|2879->1413|2926->1451|2978->1465|3023->1483|3148->1598|3200->1611|3245->1628|3288->1644|3310->1645|3351->1659|3390->1660|3419->1661|3495->1694|3534->1695|3563->1696|3616->1705|3645->1707|3667->1708|3729->1733|3765->1742|3814->1758|3846->1760
                  LINES: 37->24|42->25|42->25|42->25|43->26|43->26|43->26|44->27|47->30|47->30|48->31|48->31|48->31|48->31|48->31|48->31|48->31|49->32|49->32|49->32|49->32|50->33|50->33|50->33|51->34|52->35|52->35|52->35|52->35|53->36|53->36|53->36|54->37|54->37|54->37|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|56->39|57->40|58->41|59->42
                  -- GENERATED --
              */
          