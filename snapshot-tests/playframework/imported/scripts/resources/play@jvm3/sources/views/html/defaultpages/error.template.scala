
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object error extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template2[play.api.UsefulException,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Default page for 500 Internal Server Error responses, in production mode.
 */
  def apply/*4.2*/(error: play.api.UsefulException)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*5.1*/("""
"""),format.raw/*6.1*/("""<!DOCTYPE html>
<html lang="en">
    <head>
        <title>Error</title>
        """),_display_(/*10.10*/views/*10.15*/.html.helper.style(Symbol("type") -> "text/css")/*10.63*/ {_display_(Seq[Any](format.raw/*10.65*/("""
            """),format.raw/*11.13*/("""html, body, pre """),format.raw/*11.29*/("""{"""),format.raw/*11.30*/("""
                """),format.raw/*12.17*/("""margin: 0;
                padding: 0;
                font-family: Monaco, 'Lucida Console', monospace;
                background: #ECECEC;
            """),format.raw/*16.13*/("""}"""),format.raw/*16.14*/("""
            """),format.raw/*17.13*/("""h1 """),format.raw/*17.16*/("""{"""),format.raw/*17.17*/("""
                """),format.raw/*18.17*/("""margin: 0;
                background: #A31012;
                padding: 20px 45px;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
                border-bottom: 1px solid #690000;
                font-size: 28px;
            """),format.raw/*25.13*/("""}"""),format.raw/*25.14*/("""
            """),format.raw/*26.13*/("""p#detail """),format.raw/*26.22*/("""{"""),format.raw/*26.23*/("""
                """),format.raw/*27.17*/("""margin: 0;
                padding: 15px 45px;
                background: #F5A0A0;
                border-top: 4px solid #D36D6D;
                color: #730000;
                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
                font-size: 14px;
                border-bottom: 1px solid #BA7A7A;
            """),format.raw/*35.13*/("""}"""),format.raw/*35.14*/("""
        """)))}),format.raw/*36.10*/("""
    """),format.raw/*37.5*/("""</head>
    <body>
        <h1>Oops, an error occurred</h1>

        <p id="detail">
            This exception has been logged with id <strong>"""),_display_(/*42.61*/error/*42.66*/.id),format.raw/*42.69*/("""</strong>.
        </p>

    </body>
</html>"""))
      }
    }
  }

  def render(error:play.api.UsefulException,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(error)(request)

  def f:((play.api.UsefulException) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (error) => (request) => apply(error)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/defaultpages/error.scala.html
                  HASH: 335f72d5040a9baa98752d9038b2520d9579f4ba
                  MATRIX: 780->86|953->166|980->167|1089->249|1103->254|1160->302|1200->304|1241->317|1285->333|1314->334|1359->351|1541->505|1570->506|1611->519|1642->522|1671->523|1716->540|2009->805|2038->806|2079->819|2116->828|2145->829|2190->846|2539->1167|2568->1168|2609->1178|2641->1183|2813->1328|2827->1333|2851->1336
                  LINES: 18->4|23->5|24->6|28->10|28->10|28->10|28->10|29->11|29->11|29->11|30->12|34->16|34->16|35->17|35->17|35->17|36->18|43->25|43->25|44->26|44->26|44->26|45->27|53->35|53->35|54->36|55->37|60->42|60->42|60->42
                  -- GENERATED --
              */
          