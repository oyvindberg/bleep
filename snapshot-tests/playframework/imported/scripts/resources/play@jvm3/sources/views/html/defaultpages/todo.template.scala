
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object todo extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Default page for 501 Not Implemented responses.
 */
  def apply/*4.2*/()(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*5.1*/("""
"""),format.raw/*6.1*/("""<!DOCTYPE html>
<html>
    <head>
        <title>TODO</title>
        """),_display_(/*10.10*/views/*10.15*/.html.helper.style(Symbol("type") -> "text/css")/*10.63*/ {_display_(Seq[Any](format.raw/*10.65*/("""
            """),format.raw/*11.13*/("""html, body, pre """),format.raw/*11.29*/("""{"""),format.raw/*11.30*/("""
                """),format.raw/*12.17*/("""margin: 0;
                padding: 0;
                font-family: Monaco, 'Lucida Console', monospace;
                background: #ECECEC;
            """),format.raw/*16.13*/("""}"""),format.raw/*16.14*/("""
            """),format.raw/*17.13*/("""h1 """),format.raw/*17.16*/("""{"""),format.raw/*17.17*/("""
                """),format.raw/*18.17*/("""margin: 0;
                background: #533CAD;
                padding: 20px 45px;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
                border-bottom: 1px solid #3A0B9F;
                font-size: 28px;
            """),format.raw/*25.13*/("""}"""),format.raw/*25.14*/("""
            """),format.raw/*26.13*/("""p#detail """),format.raw/*26.22*/("""{"""),format.raw/*26.23*/("""
                """),format.raw/*27.17*/("""margin: 0;
                padding: 15px 45px;
                background: #BCACF6;
                border-top: 4px solid #7365B6;
                color: #312073;
                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
                font-size: 14px;
                border-bottom: 1px solid #39325B;
            """),format.raw/*35.13*/("""}"""),format.raw/*35.14*/("""
        """)))}),format.raw/*36.10*/("""
    """),format.raw/*37.5*/("""</head>
    <body>
        <h1>TODO</h1>

        <p id="detail">
            Action not implemented yet.
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
                  SOURCE: core/play/src/main/scala/views/defaultpages/todo.scala.html
                  HASH: 4ee9bf8161692a50dcec6780a81f51a957a1ff3e
                  MATRIX: 728->60|870->109|897->110|995->181|1009->186|1066->234|1106->236|1147->249|1191->265|1220->266|1265->283|1447->437|1476->438|1517->451|1548->454|1577->455|1622->472|1915->737|1944->738|1985->751|2022->760|2051->761|2096->778|2445->1099|2474->1100|2515->1110|2547->1115
                  LINES: 18->4|23->5|24->6|28->10|28->10|28->10|28->10|29->11|29->11|29->11|30->12|34->16|34->16|35->17|35->17|35->17|36->18|43->25|43->25|44->26|44->26|44->26|45->27|53->35|53->35|54->36|55->37
                  -- GENERATED --
              */
          