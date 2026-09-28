
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
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
                  HASH: da96cb6177a01db61c2cf5b0518972456dc83765
                  MATRIX: 760->74|929->150|1041->236|1054->241|1110->289|1149->291|1190->304|1234->320|1263->321|1308->338|1490->492|1519->493|1560->506|1591->509|1620->510|1665->527|1958->792|1987->793|2028->806|2065->815|2094->816|2139->833|2488->1154|2517->1155|2558->1165|2590->1170|2714->1267|2741->1273|2770->1275|2794->1278
                  LINES: 18->4|23->5|27->9|27->9|27->9|27->9|28->10|28->10|28->10|29->11|33->15|33->15|34->16|34->16|34->16|35->17|42->24|42->24|43->25|43->25|43->25|44->26|52->34|52->34|53->35|54->36|59->41|59->41|59->41|59->41
                  -- GENERATED --
              */
          