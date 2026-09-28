
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object unauthorized extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Default page for 401 Not Authorized responses.
 */
  def apply/*4.2*/()(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*5.1*/("""
"""),format.raw/*6.1*/("""<!DOCTYPE html>
<html lang="en">
    <head>
        <title>Unauthorized</title>
        """),_display_(/*10.10*/views/*10.15*/.html.helper.style(Symbol("type") -> "text/css")/*10.63*/ {_display_(Seq[Any](format.raw/*10.65*/("""
            """),format.raw/*11.13*/("""html, body, pre """),format.raw/*11.29*/("""{"""),format.raw/*11.30*/("""
                """),format.raw/*12.17*/("""margin: 0;
                padding: 0;
                font-family: Monaco, 'Lucida Console', monospace;
                background: #ECECEC;
            """),format.raw/*16.13*/("""}"""),format.raw/*16.14*/("""
            """),format.raw/*17.13*/("""h1 """),format.raw/*17.16*/("""{"""),format.raw/*17.17*/("""
                """),format.raw/*18.17*/("""margin: 0;
                background: #333;
                padding: 20px 45px;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
                border-bottom: 1px solid #111;
                font-size: 28px;
            """),format.raw/*25.13*/("""}"""),format.raw/*25.14*/("""
            """),format.raw/*26.13*/("""p#detail """),format.raw/*26.22*/("""{"""),format.raw/*26.23*/("""
                """),format.raw/*27.17*/("""margin: 0;
                padding: 15px 45px;
                background: #888;
                border-top: 4px solid #666;
                color: #111;
                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
                font-size: 14px;
                border-bottom: 1px solid #333;
            """),format.raw/*35.13*/("""}"""),format.raw/*35.14*/("""
        """)))}),format.raw/*36.10*/("""
    """),format.raw/*37.5*/("""</head>
    <body>
        <h1>Unauthorized</h1>
        <p id="detail">
            You must be authenticated to access this page.
        </p>
    </body>
</html>
"""))
      }
    }
  }

  def render(request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply()(request)

  def f:(() => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = () => (request) => apply()(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/defaultpages/unauthorized.scala.html
                  HASH: 1087688b06a9cb9b38d6fe09f34e6e7b74075283
                  MATRIX: 735->59|877->108|904->109|1020->198|1034->203|1091->251|1131->253|1172->266|1216->282|1245->283|1290->300|1472->454|1501->455|1542->468|1573->471|1602->472|1647->489|1934->748|1963->749|2004->762|2041->771|2070->772|2115->789|2452->1098|2481->1099|2522->1109|2554->1114
                  LINES: 18->4|23->5|24->6|28->10|28->10|28->10|28->10|29->11|29->11|29->11|30->12|34->16|34->16|35->17|35->17|35->17|36->18|43->25|43->25|44->26|44->26|44->26|45->27|53->35|53->35|54->36|55->37
                  -- GENERATED --
              */
          