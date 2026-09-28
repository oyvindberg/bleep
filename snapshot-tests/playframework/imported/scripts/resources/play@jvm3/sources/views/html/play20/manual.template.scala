
package views.html.play20

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
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
                  HASH: 796cb646f53391dc5d75ebffb73d22b4167d70c7
                  MATRIX: 697->1|879->90|939->124|964->129|1172->313|1215->336|2197->1295|2210->1299|2223->1303|2271->1313|2320->1335|2351->1345|2388->1363|2407->1373|2447->1375|2496->1396|2544->1417|2570->1422|2625->1446|2666->1459|2741->1507|2757->1514|2794->1530|2835->1543|2967->1650|3010->1672|3144->1780|3187->1801|3313->1901|3358->1924|3463->2001|3492->2002|3537->2019|3614->2068|3643->2069
                  LINES: 16->1|21->2|23->4|23->4|25->6|25->6|48->29|48->29|48->29|48->29|49->30|49->30|50->31|50->31|50->31|51->32|51->32|51->32|52->33|53->34|55->36|55->36|55->36|56->37|60->41|60->41|62->43|62->43|63->44|63->44|65->46|65->46|66->47|67->48|67->48
                  -- GENERATED --
              */
          