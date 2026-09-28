
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object badRequest extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[String,String,String,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Default page for 400 Bad Request responses.
 */
  def apply/*4.2*/(method: String, uri: String, error:String)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*5.1*/("""<!DOCTYPE html>
<html lang="en">
    <head>
        <title>Bad Request</title>
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
        <h1>Bad Request</h1>

        <p id="detail">
            For request '"""),_display_(/*41.27*/method),format.raw/*41.33*/(""" """),_display_(/*41.35*/uri),format.raw/*41.38*/("""' ["""),_display_(/*41.42*/error),format.raw/*41.47*/("""]
        </p>

    </body>
</html>
"""))
      }
    }
  }

  def render(method:String,uri:String,error:String,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(method,uri,error)(request)

  def f:((String,String,String) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (method,uri,error) => (request) => apply(method,uri,error)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/defaultpages/badRequest.scala.html
                  HASH: a6ac94fcac0527a8eacf73c0b278f7e522b859a0
                  MATRIX: 751->56|934->146|1048->234|1061->239|1117->287|1156->289|1197->302|1241->318|1270->319|1315->336|1497->490|1526->491|1567->504|1598->507|1627->508|1672->525|1965->790|1994->791|2035->804|2072->813|2101->814|2146->831|2495->1152|2524->1153|2565->1163|2597->1168|2723->1267|2750->1273|2779->1275|2803->1278|2834->1282|2860->1287
                  LINES: 18->4|23->5|27->9|27->9|27->9|27->9|28->10|28->10|28->10|29->11|33->15|33->15|34->16|34->16|34->16|35->17|42->24|42->24|43->25|43->25|43->25|44->26|52->34|52->34|53->35|54->36|59->41|59->41|59->41|59->41|59->41|59->41
                  -- GENERATED --
              */
          