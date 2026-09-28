
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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
                  HASH: b2e0704e0cb91a4d3962c30ad1c7fdc27fa7a860
                  MATRIX: 697->59|839->108|866->109|982->198|996->203|1053->251|1093->253|1134->266|1178->282|1207->283|1252->300|1434->454|1463->455|1504->468|1535->471|1564->472|1609->489|1896->748|1925->749|1966->762|2003->771|2032->772|2077->789|2414->1098|2443->1099|2484->1109|2516->1114
                  LINES: 17->4|22->5|23->6|27->10|27->10|27->10|27->10|28->11|28->11|28->11|29->12|33->16|33->16|34->17|34->17|34->17|35->18|42->25|42->25|43->26|43->26|43->26|44->27|52->35|52->35|53->36|54->37
                  -- GENERATED --
              */
          