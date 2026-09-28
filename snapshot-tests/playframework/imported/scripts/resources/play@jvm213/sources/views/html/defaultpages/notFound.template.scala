
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object notFound extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[String,String,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Default page for 404 Not Found responses, in production mode.
 */
  def apply/*4.2*/(method: String, uri: String)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*5.1*/("""<!DOCTYPE html>
<html lang="en">
    <head>
        <title>Not Found</title>
        """),_display_(/*9.10*/views/*9.15*/.html.helper.style(Symbol("type") -> "text/css")/*9.63*/ {_display_(Seq[Any](format.raw/*9.65*/("""
            """),format.raw/*10.13*/("""html, body, pre """),format.raw/*10.29*/("""{"""),format.raw/*10.30*/("""
                """),format.raw/*11.17*/("""margin: 0;
                padding: 0;
                font-family: Monaco, 'Lucida Console', monospace;
                background: #ECECEC;
            """),format.raw/*15.13*/("""}"""),format.raw/*15.14*/("""
            """),format.raw/*16.13*/("""h1 """),format.raw/*16.16*/("""{"""),format.raw/*16.17*/("""
                """),format.raw/*17.17*/("""margin: 0;
                background: #AD632A;
                padding: 20px 45px;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
                border-bottom: 1px solid #9F5805;
                font-size: 28px;
            """),format.raw/*24.13*/("""}"""),format.raw/*24.14*/("""
            """),format.raw/*25.13*/("""p#detail """),format.raw/*25.22*/("""{"""),format.raw/*25.23*/("""
                """),format.raw/*26.17*/("""margin: 0;
                padding: 15px 45px;
                background: #F6A960;
                border-top: 4px solid #D29052;
                color: #733512;
                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
                font-size: 14px;
                border-bottom: 1px solid #BA7F5B;
            """),format.raw/*34.13*/("""}"""),format.raw/*34.14*/("""
        """)))}),format.raw/*35.10*/("""
    """),format.raw/*36.5*/("""</head>
    <body>
        <h1>Not Found</h1>

        <p id="detail">
            For request '"""),_display_(/*41.27*/method),format.raw/*41.33*/(""" """),_display_(/*41.35*/uri),format.raw/*41.38*/("""'
        </p>

    </body>
</html>
"""))
      }
    }
  }

  def render(method:String,uri:String,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(method,uri)(request)

  def f:((String,String) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (method,uri) => (request) => apply(method,uri)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/defaultpages/notFound.scala.html
                  HASH: 03a1cf7f522fd09df0170189ee6dde0f40dd10e5
                  MATRIX: 722->74|891->150|1003->236|1016->241|1072->289|1111->291|1152->304|1196->320|1225->321|1270->338|1452->492|1481->493|1522->506|1553->509|1582->510|1627->527|1920->792|1949->793|1990->806|2027->815|2056->816|2101->833|2450->1154|2479->1155|2520->1165|2552->1170|2676->1267|2703->1273|2732->1275|2756->1278
                  LINES: 17->4|22->5|26->9|26->9|26->9|26->9|27->10|27->10|27->10|28->11|32->15|32->15|33->16|33->16|33->16|34->17|41->24|41->24|42->25|42->25|42->25|43->26|51->34|51->34|52->35|53->36|58->41|58->41|58->41|58->41
                  -- GENERATED --
              */
          