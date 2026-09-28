
package views.html.play20

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object manual extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[String,Option[String],Option[String],String => String,play.twirl.api.HtmlFormat.Appendable] {

  /**/
  def apply/*1.2*/(title: String, main: Option[String], sidebar: Option[String], locate: String => String):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*2.1*/("""<html>
    <head>
        <title>"""),_display_(/*4.17*/title),format.raw/*4.22*/("""</title>
        <link rel="stylesheet" media="screen" href="/@documentation/resources/style/main.css"></link>
        <script type="text/javascript" src='/@documentation/resources/"""),_display_(/*6.73*/locate("jquery.min.js")),format.raw/*6.96*/("""'></script>
        <script type="text/javascript" src="/@documentation/resources/style/main.js"></script>
    </head>
    <body>

        <section id="top">
            <div class="wrapper">
                <h1><a href="/@documentation">Manual, tutorials & references</a></h1>
                <nav>
                    <span class="versions">
                        <span>Browse APIs</span>
                        <select onchange="document.location=this.value">
                            <option selected disabled>Select language</option>
                            <option value="/@documentation/api/scala/index.html">Scala</option>
                            <option value="/@documentation/api/java/index.html">Java</option>
                        </select>
                    </span>
                </nav>
            </div>
        </section>

        <div id="content" class="wrapper doc">
            <article id="main">
                """),_display_(/*29.18*/main/*29.22*/.map/*29.26*/ { html =>_display_(Seq[Any](format.raw/*29.36*/("""
                    """),_display_(/*30.22*/Html(html)),format.raw/*30.32*/("""
                """)))}/*31.18*/.getOrElse/*31.28*/ {_display_(Seq[Any](format.raw/*31.30*/("""
                    """),format.raw/*32.21*/("""<h1>Page not found ["""),_display_(/*32.42*/title),format.raw/*32.47*/("""]</h1>
                """)))}),format.raw/*33.18*/("""
            """),format.raw/*34.13*/("""</article>
            <aside>
                """),_display_(/*36.18*/sidebar/*36.25*/.map(Html.apply)),format.raw/*36.41*/("""
            """),format.raw/*37.13*/("""</aside>
        </div>

        <style type="text/css">
            @import '/@documentation/resources/"""),_display_(/*41.51*/locate("prettify.css")),format.raw/*41.73*/("""';
        </style>
        <script type="text/javascript" charset="utf-8" src='/@documentation/resources/"""),_display_(/*43.89*/locate("prettify.js")),format.raw/*43.110*/("""'></script>
        <script type="text/javascript" charset="utf-8" src='/@documentation/resources/"""),_display_(/*44.89*/locate("lang-scala.js")),format.raw/*44.112*/("""'></script>
        <script type="text/javascript">
            $(function() """),format.raw/*46.26*/("""{"""),format.raw/*46.27*/("""
                """),format.raw/*47.17*/("""window.prettyPrint && prettyPrint();
            """),format.raw/*48.13*/("""}"""),format.raw/*48.14*/(""");
        </script>

    </body>
</html>
"""))
      }
    }
  }

  def render(title:String,main:Option[String],sidebar:Option[String],locate:String => String): play.twirl.api.HtmlFormat.Appendable = apply(title,main,sidebar,locate)

  def f:((String,Option[String],Option[String],String => String) => play.twirl.api.HtmlFormat.Appendable) = (title,main,sidebar,locate) => apply(title,main,sidebar,locate)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/play20/manual.scala.html
                  HASH: e4641dc1709211620be0e479561432317149dbaa
                  MATRIX: 659->1|841->90|901->124|926->129|1134->313|1177->336|2159->1295|2172->1299|2185->1303|2233->1313|2282->1335|2313->1345|2350->1363|2369->1373|2409->1375|2458->1396|2506->1417|2532->1422|2587->1446|2628->1459|2703->1507|2719->1514|2756->1530|2797->1543|2929->1650|2972->1672|3106->1780|3149->1801|3275->1901|3320->1924|3425->2001|3454->2002|3499->2019|3576->2068|3605->2069
                  LINES: 15->1|20->2|22->4|22->4|24->6|24->6|47->29|47->29|47->29|47->29|48->30|48->30|49->31|49->31|49->31|50->32|50->32|50->32|51->33|52->34|54->36|54->36|54->36|55->37|59->41|59->41|61->43|61->43|62->44|62->44|64->46|64->46|65->47|66->48|66->48
                  -- GENERATED --
              */
          