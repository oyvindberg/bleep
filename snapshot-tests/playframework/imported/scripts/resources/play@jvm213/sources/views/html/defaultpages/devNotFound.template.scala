
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
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
                  HASH: d30064cab3d05ba9bcd3c85b52f54d325541bbfd
                  MATRIX: 804->121|1014->238|2115->1312|2129->1317|2186->1365|2226->1367|2267->1380|2311->1396|2340->1397|2385->1414|2567->1568|2596->1569|2637->1582|2668->1585|2697->1586|2742->1603|3035->1868|3064->1869|3105->1882|3142->1891|3171->1892|3216->1909|3565->2230|3594->2231|3635->2244|3666->2247|3695->2248|3740->2265|4026->2523|4055->2524|4096->2537|4128->2541|4157->2542|4202->2559|4432->2761|4461->2762|4502->2775|4544->2789|4573->2790|4618->2807|4946->3107|4975->3108|5016->3121|5059->3136|5088->3137|5133->3154|5281->3274|5310->3275|5351->3288|5404->3313|5433->3314|5478->3331|5671->3496|5700->3497|5741->3510|5794->3535|5823->3536|5868->3553|6063->3720|6092->3721|6133->3734|6186->3759|6215->3760|6260->3777|6421->3910|6450->3911|6491->3924|6546->3951|6575->3952|6620->3969|6691->4012|6720->4013|6761->4026|6815->4052|6844->4053|6889->4070|6960->4113|6989->4114|7030->4127|7078->4147|7107->4148|7152->4165|7299->4284|7328->4285|7370->4295|7403->4300|7535->4404|7563->4410|7593->4412|7618->4415|7671->4440|7687->4446|7703->4452|7715->4468|7745->4488|7786->4490|7833->4508|7997->4644|8013->4650|8054->4681|8100->4688|8154->4713|8207->4738|8239->4748|8315->4795|8327->4796|8356->4802|8412->4829|8424->4830|8453->4836|8509->4863|8521->4864|8550->4870|8624->4912|8670->4929|8711->4964|8733->4976|8774->4978|8820->4995|8932->5085|8966->5091
                  LINES: 18->5|23->6|28->11|28->11|28->11|28->11|29->12|29->12|29->12|30->13|34->17|34->17|35->18|35->18|35->18|36->19|43->26|43->26|44->27|44->27|44->27|45->28|53->36|53->36|54->37|54->37|54->37|55->38|62->45|62->45|63->46|63->46|63->46|64->47|69->52|69->52|70->53|70->53|70->53|71->54|79->62|79->62|80->63|80->63|80->63|81->64|85->68|85->68|86->69|86->69|86->69|87->70|92->75|92->75|93->76|93->76|93->76|94->77|99->82|99->82|100->83|100->83|100->83|101->84|105->88|105->88|106->89|106->89|106->89|107->90|108->91|108->91|109->92|109->92|109->92|110->93|111->94|111->94|112->95|112->95|112->95|113->96|116->99|116->99|117->100|118->101|123->106|123->106|123->106|123->106|126->109|126->109|126->109|126->111|126->111|126->111|128->113|133->118|133->118|133->118|133->118|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|135->120|136->121|138->125|138->125|138->125|139->126|142->131|144->133
                  -- GENERATED --
              */
          