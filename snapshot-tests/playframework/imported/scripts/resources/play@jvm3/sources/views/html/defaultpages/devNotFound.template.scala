
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object devNotFound extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[String,String,Option[play.api.routing.Router],play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Default page for 404 Not Found responses, in development mode.
 * This page display the routes file content.
 */
  def apply/*5.2*/(method: String, uri: String, router: Option[play.api.routing.Router])(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*6.1*/("""<!DOCTYPE html>
<html lang="en">
    <head>
        <title>Action Not Found</title>
        <link rel="shortcut icon" href="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAlFJREFUeNqUU8tOFEEUPVVdNV3dPe8xYRBnjGhmBgKjKzCIiQvBoIaNbly5Z+PSv3Aj7DSiP2B0rwkLGVdGgxITSCRIJGSMEQWZR3eVt5sEFBgTb/dN1yvnnHtPNTPG4PqdHgCMXnPRSZrpSuH8vUJu4DE4rYHDGAZDX62BZttHqTiIayM3gGiXQsgYLEvATaqxU+dy1U13YXapXptpNHY8iwn8KyIAzm1KBdtRZWErpI5lEWTXp5Z/vHpZ3/wyKKwYGGOdAYwR0EZwoezTYApBEIObyELl/aE1/83cp40Pt5mxqCKrE4Ck+mVWKKcI5tA8BLEhRBKJLjez6a7MLq7XZtp+yyOawwCBtkiBVZDKzRk4NN7NQBMYPHiZDFhXY+p9ff7F961vVcnl4R5I2ykJ5XFN7Ab7Gc61VoipNBKF+PDyztu5lfrSLT/wIwCxq0CAGtXHZTzqR2jtwQiXONma6hHpj9sLT7YaPxfTXuZdBGA02Wi7FS48YiTfj+i2NhqtdhP5RC8mh2/Op7y0v6eAcWVLFT8D7kWX5S9mepp+C450MV6aWL1cGnvkxbwHtLW2B9AOkLeUd9KEDuh9fl/7CEj7YH5g+3r/lWfF9In7tPz6T4IIwBJOr1SJyIGQMZQbsh5P9uBq5VJtqHh2mo49pdw5WFoEwKWqWHacaWOjQXWGcifKo6vj5RGS6zykI587XeUIQDqJSmAp+lE4qt19W5P9o8+Lma5DcjsC8JiT607lMVkdqQ0Vyh3lHhmh52tfNy78ajXv0rgYzv8nfwswANuk+7sD/Q0aAAAAAElFTkSuQmCC">
        """),_display_(/*11.10*/views/*11.15*/.html.helper.style(Symbol("type") -> "text/css")/*11.63*/ {_display_(Seq[Any](format.raw/*11.65*/("""
            """),format.raw/*12.13*/("""html, body, pre """),format.raw/*12.29*/("""{"""),format.raw/*12.30*/("""
                """),format.raw/*13.17*/("""margin: 0;
                padding: 0;
                font-family: Monaco, 'Lucida Console', monospace;
                background: #ECECEC;
            """),format.raw/*17.13*/("""}"""),format.raw/*17.14*/("""
            """),format.raw/*18.13*/("""h1 """),format.raw/*18.16*/("""{"""),format.raw/*18.17*/("""
                """),format.raw/*19.17*/("""margin: 0;
                background: #AD632A;
                padding: 20px 45px;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
                border-bottom: 1px solid #9F5805;
                font-size: 28px;
            """),format.raw/*26.13*/("""}"""),format.raw/*26.14*/("""
            """),format.raw/*27.13*/("""p#detail """),format.raw/*27.22*/("""{"""),format.raw/*27.23*/("""
                """),format.raw/*28.17*/("""margin: 0;
                padding: 15px 45px;
                background: #F6A960;
                border-top: 4px solid #D29052;
                color: #733512;
                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
                font-size: 14px;
                border-bottom: 1px solid #BA7F5B;
            """),format.raw/*36.13*/("""}"""),format.raw/*36.14*/("""
            """),format.raw/*37.13*/("""h2 """),format.raw/*37.16*/("""{"""),format.raw/*37.17*/("""
                """),format.raw/*38.17*/("""margin: 0;
                padding: 5px 45px;
                font-size: 12px;
                background: #333;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
                border-top: 4px solid #2a2a2a;
            """),format.raw/*45.13*/("""}"""),format.raw/*45.14*/("""
            """),format.raw/*46.13*/("""pre """),format.raw/*46.17*/("""{"""),format.raw/*46.18*/("""
                """),format.raw/*47.17*/("""margin: 0;
                border-bottom: 1px solid #DDD;
                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
                position: relative;
                font-size: 12px;
            """),format.raw/*52.13*/("""}"""),format.raw/*52.14*/("""
            """),format.raw/*53.13*/("""pre span.line """),format.raw/*53.27*/("""{"""),format.raw/*53.28*/("""
                """),format.raw/*54.17*/("""text-align: right;
                display: inline-block;
                padding: 5px 5px;
                width: 30px;
                background: #D6D6D6;
                color: #8B8B8B;
                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
                font-weight: bold;
            """),format.raw/*62.13*/("""}"""),format.raw/*62.14*/("""
            """),format.raw/*63.13*/("""pre span.route """),format.raw/*63.28*/("""{"""),format.raw/*63.29*/("""
                """),format.raw/*64.17*/("""padding: 5px 5px;
                position: absolute;
                right: 0;
                left: 40px;
            """),format.raw/*68.13*/("""}"""),format.raw/*68.14*/("""
            """),format.raw/*69.13*/("""pre span.route span.verb """),format.raw/*69.38*/("""{"""),format.raw/*69.39*/("""
                """),format.raw/*70.17*/("""display: inline-block;
                width: 5%;
                min-width: 50px;
                overflow: hidden;
                margin-right: 10px;
            """),format.raw/*75.13*/("""}"""),format.raw/*75.14*/("""
            """),format.raw/*76.13*/("""pre span.route span.path """),format.raw/*76.38*/("""{"""),format.raw/*76.39*/("""
                """),format.raw/*77.17*/("""display: inline-block;
                width: 30%;
                min-width: 200px;
                overflow: hidden;
                margin-right: 10px;
            """),format.raw/*82.13*/("""}"""),format.raw/*82.14*/("""
            """),format.raw/*83.13*/("""pre span.route span.call """),format.raw/*83.38*/("""{"""),format.raw/*83.39*/("""
                """),format.raw/*84.17*/("""display: inline-block;
                width: 50%;
                overflow: hidden;
                margin-right: 10px;
            """),format.raw/*88.13*/("""}"""),format.raw/*88.14*/("""
            """),format.raw/*89.13*/("""pre:first-child span.route """),format.raw/*89.40*/("""{"""),format.raw/*89.41*/("""
                """),format.raw/*90.17*/("""border-top: 4px solid #CDCDCD;
            """),format.raw/*91.13*/("""}"""),format.raw/*91.14*/("""
            """),format.raw/*92.13*/("""pre:first-child span.line """),format.raw/*92.39*/("""{"""),format.raw/*92.40*/("""
                """),format.raw/*93.17*/("""border-top: 4px solid #B6B6B6;
            """),format.raw/*94.13*/("""}"""),format.raw/*94.14*/("""
            """),format.raw/*95.13*/("""pre.error span.line """),format.raw/*95.33*/("""{"""),format.raw/*95.34*/("""
                """),format.raw/*96.17*/("""background: #A31012;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
            """),format.raw/*99.13*/("""}"""),format.raw/*99.14*/("""
        """)))}),format.raw/*100.10*/("""
    """),format.raw/*101.5*/("""</head>
    <body>
        <h1>Action Not Found</h1>

        <p id="detail">
            For request '"""),_display_(/*106.27*/method),format.raw/*106.33*/(""" """),_display_(/*106.35*/uri),format.raw/*106.38*/("""'
        </p>

        """),_display_(/*109.10*/router/*109.16*/ match/*109.22*/ {/*111.13*/case Some(routes) =>/*111.33*/ {_display_(Seq[Any](format.raw/*111.35*/("""

                """),format.raw/*113.17*/("""<h2>
                    These routes have been tried, in this order:
                </h2>

                <div>
                    """),_display_(/*118.22*/routes/*118.28*/.documentation.zipWithIndex.map/*118.59*/ { r =>_display_(Seq[Any](format.raw/*118.66*/("""
                        """),format.raw/*119.25*/("""<pre><span class="line">"""),_display_(/*119.50*/(r._2 + 1)),format.raw/*119.60*/("""</span><span class="route"><span class="verb">"""),_display_(/*119.107*/r/*119.108*/._1._1),format.raw/*119.114*/("""</span><span class="path">"""),_display_(/*119.141*/r/*119.142*/._1._2),format.raw/*119.148*/("""</span><span class="call">"""),_display_(/*119.175*/r/*119.176*/._1._3),format.raw/*119.182*/("""</span></span></pre>
                    """)))}),format.raw/*120.22*/("""
                """),format.raw/*121.17*/("""</div>

            """)))}/*125.13*/case None =>/*125.25*/ {_display_(Seq[Any](format.raw/*125.27*/("""
                """),format.raw/*126.17*/("""<h2>
                    No router defined.
                </h2>
            """)))}}),format.raw/*131.10*/("""

    """),format.raw/*133.5*/("""</body>
</html>
"""))
      }
    }
  }

  def render(method:String,uri:String,router:Option[play.api.routing.Router],request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(method,uri,router)(request)

  def f:((String,String,Option[play.api.routing.Router]) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (method,uri,router) => (request) => apply(method,uri,router)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/defaultpages/devNotFound.scala.html
                  HASH: 30701b83c9e383f3c65cf06911ce4ded9518e807
                  MATRIX: 842->121|1052->238|2153->1312|2167->1317|2224->1365|2264->1367|2305->1380|2349->1396|2378->1397|2423->1414|2605->1568|2634->1569|2675->1582|2706->1585|2735->1586|2780->1603|3073->1868|3102->1869|3143->1882|3180->1891|3209->1892|3254->1909|3603->2230|3632->2231|3673->2244|3704->2247|3733->2248|3778->2265|4064->2523|4093->2524|4134->2537|4166->2541|4195->2542|4240->2559|4470->2761|4499->2762|4540->2775|4582->2789|4611->2790|4656->2807|4984->3107|5013->3108|5054->3121|5097->3136|5126->3137|5171->3154|5319->3274|5348->3275|5389->3288|5442->3313|5471->3314|5516->3331|5709->3496|5738->3497|5779->3510|5832->3535|5861->3536|5906->3553|6101->3720|6130->3721|6171->3734|6224->3759|6253->3760|6298->3777|6459->3910|6488->3911|6529->3924|6584->3951|6613->3952|6658->3969|6729->4012|6758->4013|6799->4026|6853->4052|6882->4053|6927->4070|6998->4113|7027->4114|7068->4127|7116->4147|7145->4148|7190->4165|7337->4284|7366->4285|7408->4295|7441->4300|7573->4404|7601->4410|7631->4412|7656->4415|7709->4440|7725->4446|7741->4452|7753->4468|7783->4488|7824->4490|7871->4508|8035->4644|8051->4650|8092->4681|8138->4688|8192->4713|8245->4738|8277->4748|8353->4795|8365->4796|8394->4802|8450->4829|8462->4830|8491->4836|8547->4863|8559->4864|8588->4870|8662->4912|8708->4929|8749->4964|8771->4976|8812->4978|8858->4995|8970->5085|9004->5091
                  LINES: 19->5|24->6|29->11|29->11|29->11|29->11|30->12|30->12|30->12|31->13|35->17|35->17|36->18|36->18|36->18|37->19|44->26|44->26|45->27|45->27|45->27|46->28|54->36|54->36|55->37|55->37|55->37|56->38|63->45|63->45|64->46|64->46|64->46|65->47|70->52|70->52|71->53|71->53|71->53|72->54|80->62|80->62|81->63|81->63|81->63|82->64|86->68|86->68|87->69|87->69|87->69|88->70|93->75|93->75|94->76|94->76|94->76|95->77|100->82|100->82|101->83|101->83|101->83|102->84|106->88|106->88|107->89|107->89|107->89|108->90|109->91|109->91|110->92|110->92|110->92|111->93|112->94|112->94|113->95|113->95|113->95|114->96|117->99|117->99|118->100|119->101|124->106|124->106|124->106|124->106|127->109|127->109|127->109|127->111|127->111|127->111|129->113|134->118|134->118|134->118|134->118|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|136->120|137->121|139->125|139->125|139->125|140->126|143->131|145->133
                  -- GENERATED --
              */
          