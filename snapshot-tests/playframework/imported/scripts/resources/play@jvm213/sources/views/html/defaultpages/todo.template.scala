
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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
                  HASH: e6e3a6b48b6285dd950ff9c5b09d0d897e161d9e
                  MATRIX: 690->60|832->109|859->110|957->181|971->186|1028->234|1068->236|1109->249|1153->265|1182->266|1227->283|1409->437|1438->438|1479->451|1510->454|1539->455|1584->472|1877->737|1906->738|1947->751|1984->760|2013->761|2058->778|2407->1099|2436->1100|2477->1110|2509->1115
                  LINES: 17->4|22->5|23->6|27->10|27->10|27->10|27->10|28->11|28->11|28->11|29->12|33->16|33->16|34->17|34->17|34->17|35->18|42->25|42->25|43->26|43->26|43->26|44->27|52->35|52->35|53->36|54->37
                  -- GENERATED --
              */
          