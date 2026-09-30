
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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
                  HASH: bc260cdda4d635148bce2722e0d259beefde17e5
                  MATRIX: 742->86|915->166|942->167|1051->249|1065->254|1122->302|1162->304|1203->317|1247->333|1276->334|1321->351|1503->505|1532->506|1573->519|1604->522|1633->523|1678->540|1971->805|2000->806|2041->819|2078->828|2107->829|2152->846|2501->1167|2530->1168|2571->1178|2603->1183|2775->1328|2789->1333|2813->1336
                  LINES: 17->4|22->5|23->6|27->10|27->10|27->10|27->10|28->11|28->11|28->11|29->12|33->16|33->16|34->17|34->17|34->17|35->18|42->25|42->25|43->26|43->26|43->26|44->27|52->35|52->35|53->36|54->37|59->42|59->42|59->42
                  -- GENERATED --
              */
          