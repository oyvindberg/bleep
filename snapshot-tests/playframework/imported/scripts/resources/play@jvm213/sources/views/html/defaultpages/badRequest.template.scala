
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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
                  HASH: 848647593c0d741eba3309cb5c95a902446845db
                  MATRIX: 713->56|896->146|1010->234|1023->239|1079->287|1118->289|1159->302|1203->318|1232->319|1277->336|1459->490|1488->491|1529->504|1560->507|1589->508|1634->525|1927->790|1956->791|1997->804|2034->813|2063->814|2108->831|2457->1152|2486->1153|2527->1163|2559->1168|2685->1267|2712->1273|2741->1275|2765->1278|2796->1282|2822->1287
                  LINES: 17->4|22->5|26->9|26->9|26->9|26->9|27->10|27->10|27->10|28->11|32->15|32->15|33->16|33->16|33->16|34->17|41->24|41->24|42->25|42->25|42->25|43->26|51->34|51->34|52->35|53->36|58->41|58->41|58->41|58->41|58->41|58->41
                  -- GENERATED --
              */
          