
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object devError extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[Option[String],play.api.UsefulException,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Default page for 500 Internal Server Error responses, in development mode.
 * This page display the error in the source code context.
 */
  def apply/*5.2*/(playEditor: Option[String], error: play.api.UsefulException)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*6.1*/("""<!DOCTYPE html>
<html lang="en">
    <head>
        <title>"""),_display_(/*9.17*/error/*9.22*/.title),format.raw/*9.28*/("""</title>
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
                background: #A31012;
                padding: 20px 45px;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
                border-bottom: 1px solid #690000;
                font-size: 28px;
            """),format.raw/*26.13*/("""}"""),format.raw/*26.14*/("""
            """),format.raw/*27.13*/("""a """),format.raw/*27.15*/("""{"""),format.raw/*27.16*/("""
                """),format.raw/*28.17*/("""color: #D36D6D;
            """),format.raw/*29.13*/("""}"""),format.raw/*29.14*/("""
            """),format.raw/*30.13*/("""p#detail """),format.raw/*30.22*/("""{"""),format.raw/*30.23*/("""
                """),format.raw/*31.17*/("""margin: 0;
                padding: 15px 45px;
                background: #F5A0A0;
                border-top: 4px solid #D36D6D;
                color: #730000;
                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
                font-size: 14px;
                border-bottom: 1px solid #BA7A7A;
            """),format.raw/*39.13*/("""}"""),format.raw/*39.14*/("""
            """),format.raw/*40.13*/("""p#detail.pre """),format.raw/*40.26*/("""{"""),format.raw/*40.27*/("""
                """),format.raw/*41.17*/("""white-space: pre;
                font-size: 13px;
                overflow: auto;
            """),format.raw/*44.13*/("""}"""),format.raw/*44.14*/("""
            """),format.raw/*45.13*/("""p#detail input """),format.raw/*45.28*/("""{"""),format.raw/*45.29*/("""
                """),format.raw/*46.17*/("""background: #AE1113;
                background: -webkit-linear-gradient(#AE1113, #A31012);
                background: -o-linear-gradient(#AE1113, #A31012);
                background: -moz-linear-gradient(#AE1113, #A31012);
                background: linear-gradient(#AE1113, #A31012);
                border: 1px solid #790000;
                padding: 3px 10px;
                text-shadow: 1px 1px 0 rgba(0, 0, 0, .5);
                color: white;
                border-radius: 3px;
                cursor: pointer;
                font-family: Monaco, 'Lucida Console';
                font-size: 12px;
                margin: 0 10px;
                display: inline-block;
                position: relative;
                top: -1px;
            """),format.raw/*63.13*/("""}"""),format.raw/*63.14*/("""
            """),format.raw/*64.13*/("""h2 """),format.raw/*64.16*/("""{"""),format.raw/*64.17*/("""
                """),format.raw/*65.17*/("""margin: 0;
                padding: 5px 45px;
                font-size: 12px;
                background: #333;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
                border-top: 4px solid #2a2a2a;
            """),format.raw/*72.13*/("""}"""),format.raw/*72.14*/("""
            """),format.raw/*73.13*/("""pre """),format.raw/*73.17*/("""{"""),format.raw/*73.18*/("""
                """),format.raw/*74.17*/("""margin: 0;
                border-bottom: 1px solid #DDD;
                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
                position: relative;
                font-size: 12px;
            """),format.raw/*79.13*/("""}"""),format.raw/*79.14*/("""
            """),format.raw/*80.13*/("""pre span.line """),format.raw/*80.27*/("""{"""),format.raw/*80.28*/("""
                """),format.raw/*81.17*/("""text-align: right;
                display: inline-block;
                padding: 5px 5px;
                width: 30px;
                background: #D6D6D6;
                color: #8B8B8B;
                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
                font-weight: bold;
            """),format.raw/*89.13*/("""}"""),format.raw/*89.14*/("""
            """),format.raw/*90.13*/("""pre span.code """),format.raw/*90.27*/("""{"""),format.raw/*90.28*/("""
                """),format.raw/*91.17*/("""padding: 5px 5px;
                position: absolute;
                right: 0;
                left: 40px;
            """),format.raw/*95.13*/("""}"""),format.raw/*95.14*/("""
            """),format.raw/*96.13*/("""pre:first-child span.code """),format.raw/*96.39*/("""{"""),format.raw/*96.40*/("""
                """),format.raw/*97.17*/("""border-top: 4px solid #CDCDCD;
            """),format.raw/*98.13*/("""}"""),format.raw/*98.14*/("""
            """),format.raw/*99.13*/("""pre:first-child span.line """),format.raw/*99.39*/("""{"""),format.raw/*99.40*/("""
                """),format.raw/*100.17*/("""border-top: 4px solid #B6B6B6;
            """),format.raw/*101.13*/("""}"""),format.raw/*101.14*/("""
            """),format.raw/*102.13*/("""pre.error span.line """),format.raw/*102.33*/("""{"""),format.raw/*102.34*/("""
                """),format.raw/*103.17*/("""background: #A31012;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
            """),format.raw/*106.13*/("""}"""),format.raw/*106.14*/("""
            """),format.raw/*107.13*/("""pre.error """),format.raw/*107.23*/("""{"""),format.raw/*107.24*/("""
                """),format.raw/*108.17*/("""color: #A31012;
            """),format.raw/*109.13*/("""}"""),format.raw/*109.14*/("""
            """),format.raw/*110.13*/("""pre.error span.marker """),format.raw/*110.35*/("""{"""),format.raw/*110.36*/("""
                """),format.raw/*111.17*/("""background: #A31012;
                color: #fff;
                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
            """),format.raw/*114.13*/("""}"""),format.raw/*114.14*/("""
        """)))}),format.raw/*115.10*/("""
    """),format.raw/*116.5*/("""</head>
    <body id="play-error-page">
        <h1>"""),_display_(/*118.14*/error/*118.19*/.title),format.raw/*118.25*/("""</h1>

        """),_display_(/*120.10*/error/*120.15*/ match/*120.21*/ {/*122.13*/case description:play.api.PlayException.RichDescription =>/*122.71*/ {_display_(Seq[Any](format.raw/*122.73*/("""
                """),format.raw/*123.17*/("""<p id="detail">"""),_display_(/*123.33*/play/*123.37*/.twirl.api.Html(description.htmlDescription)),format.raw/*123.81*/("""</p>
            """)))}/*126.13*/case _ =>/*126.22*/ {_display_(Seq[Any](format.raw/*126.24*/("""
                """),format.raw/*127.17*/("""<p id="detail" class="pre">"""),_display_(/*127.45*/error/*127.50*/.description),format.raw/*127.62*/("""</p>
            """)))}}),format.raw/*130.10*/("""

        """),_display_(/*132.10*/error/*132.15*/ match/*132.21*/ {/*134.13*/case source:play.api.PlayException.ExceptionSource =>/*134.66*/ {_display_(Seq[Any](format.raw/*134.68*/("""

                """),_display_(/*136.18*/Option(source.sourceName)/*136.43*/.map/*136.47*/ { name =>_display_(Seq[Any](format.raw/*136.57*/("""
                    """),format.raw/*137.21*/("""<h2>
                        In """),_display_(/*138.29*/Option(source.line)/*138.48*/.fold/*138.53*/ {_display_(Seq[Any](format.raw/*138.55*/("""
                          """),_display_(/*139.28*/name),format.raw/*139.32*/(""" """),format.raw/*139.33*/("""(line number not found)
                        """)))}/*140.26*/{line =>_display_(Seq[Any](format.raw/*140.34*/("""
                          """),_display_(/*141.28*/playEditor/*141.38*/.fold/*141.43*/ {_display_(Seq[Any](format.raw/*141.45*/("""
                            """),_display_(/*142.30*/name),format.raw/*142.34*/(""":"""),_display_(/*142.36*/line),format.raw/*142.40*/("""
                          """)))}/*143.28*/ { link =>_display_(Seq[Any](format.raw/*143.38*/("""
                            """),format.raw/*144.29*/("""<iframe name="_onlyForFiringEditorLink" style="display:none;"></iframe>
                            <a href=""""),_display_(/*145.39*/{link.format(name, line)}),format.raw/*145.64*/("""" target="_onlyForFiringEditorLink">"""),_display_(/*145.101*/name),format.raw/*145.105*/(""":"""),_display_(/*145.107*/line),format.raw/*145.111*/("""</a>
                          """)))}),format.raw/*146.28*/("""
                        """)))}),format.raw/*147.26*/("""
                    """),format.raw/*148.21*/("""</h2>

                    <div id="source-code">
                        """),_display_(/*151.26*/Option(source.interestingLines(4))/*151.60*/.map/*151.64*/ {/*153.29*/case interesting =>/*153.48*/ {_display_(Seq[Any](format.raw/*153.50*/("""

                                """),_display_(/*155.34*/interesting/*155.45*/.focus.zipWithIndex.map/*155.68*/ {/*157.37*/case (line,index) if index == interesting.errorLine =>/*157.91*/ {_display_(Seq[Any](format.raw/*157.93*/("""
                                        """),format.raw/*158.41*/("""<pre class="error" data-file=""""),_display_(/*158.72*/name),format.raw/*158.76*/("""" data-line=""""),_display_(/*158.90*/(interesting.firstLine+index)),format.raw/*158.119*/("""" """),_display_(/*158.122*/Option(source.position)/*158.145*/.map/*158.149*/ { c =>_display_(Seq[Any](format.raw/*158.156*/(""" """),format.raw/*158.157*/("""data-column=""""),_display_(/*158.171*/c),format.raw/*158.172*/("""" """)))}),format.raw/*158.175*/("""><span class="line">"""),_display_(/*158.196*/(interesting.firstLine+index)),format.raw/*158.225*/("""</span><span class="code">"""),_display_(/*158.252*/(Option(source.position).map(pos => (line+" ").zipWithIndex.map{ case (c,i) if i == pos => Html("""<span class="marker">""" + c + """</span>"""); case (c,_) => c}).getOrElse(line))),format.raw/*158.432*/("""</span></pre>

                                    """)))}/*162.37*/case (line, index) =>/*162.58*/ {_display_(Seq[Any](format.raw/*162.60*/("""
                                        """),format.raw/*163.41*/("""<pre data-file=""""),_display_(/*163.58*/name),format.raw/*163.62*/("""" data-line=""""),_display_(/*163.76*/(interesting.firstLine+index)),format.raw/*163.105*/(""""><span class="line">"""),_display_(/*163.127*/(interesting.firstLine+index)),format.raw/*163.156*/("""</span><span class="code">"""),_display_(/*163.183*/line),format.raw/*163.187*/("""</span></pre>
                                    """)))}}),format.raw/*166.34*/("""
                            """)))}}),format.raw/*169.26*/("""
                    """),format.raw/*170.21*/("""</div>

                """)))}),format.raw/*172.18*/("""

            """)))}/*176.13*/case attachment:play.api.PlayException.ExceptionAttachment =>/*176.74*/ {_display_(Seq[Any](format.raw/*176.76*/("""

                """),format.raw/*178.17*/("""<h2>"""),_display_(/*178.22*/attachment/*178.32*/.subTitle),format.raw/*178.41*/("""</h2>

                <div>
                    """),_display_(/*181.22*/attachment/*181.32*/.content.split("\n").zipWithIndex.map/*181.69*/ {/*183.25*/case (line,index) =>/*183.45*/ {_display_(Seq[Any](format.raw/*183.47*/("""
                            """),format.raw/*184.29*/("""<pre><span class="line">"""),_display_(/*184.54*/(index+1)),format.raw/*184.63*/("""</span><span class="code">"""),_display_(/*184.90*/line),format.raw/*184.94*/("""</span></pre>
                        """)))}}),format.raw/*187.22*/("""
                """),format.raw/*188.17*/("""</div>

            """)))}/*192.13*/case exception: play.api.PlayException if exception.cause != null =>/*192.81*/ {_display_(Seq[Any](format.raw/*192.83*/("""

                """),format.raw/*194.17*/("""<h2>
                    No source available, here is the exception stack trace:
                </h2>

                <div>

                    <pre class="error"><span class="line">-></span><span class="code">"""),_display_(/*200.88*/exception/*200.97*/.cause.getClass.getName),format.raw/*200.120*/(""": """),_display_(/*200.123*/exception/*200.132*/.cause.getMessage),format.raw/*200.149*/("""</span></pre>

                    """),_display_(/*202.22*/exception/*202.31*/.cause.getStackTrace.map/*202.55*/ { line =>_display_(Seq[Any](format.raw/*202.65*/("""
                        """),format.raw/*203.25*/("""<pre><span class="line">&nbsp;</span><span class="code">    """),_display_(/*203.86*/line),format.raw/*203.90*/("""</span></pre>
                    """)))}),format.raw/*204.22*/("""
                """),format.raw/*205.17*/("""</div>

            """)))}/*209.13*/case _ =>/*209.22*/ {_display_(Seq[Any](format.raw/*209.24*/("""
            """)))}}),format.raw/*212.10*/("""

    """),format.raw/*214.5*/("""</body>
</html>
"""))
      }
    }
  }

  def render(playEditor:Option[String],error:play.api.UsefulException,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(playEditor,error)(request)

  def f:((Option[String],play.api.UsefulException) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (playEditor,error) => (request) => apply(playEditor,error)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/defaultpages/devError.scala.html
                  HASH: c3f1ff88a7417fee8c441502ff83604d7abb536a
                  MATRIX: 820->146|1021->254|1107->314|1120->319|1146->325|2172->1324|2186->1329|2243->1377|2283->1379|2324->1392|2368->1408|2397->1409|2442->1426|2624->1580|2653->1581|2694->1594|2725->1597|2754->1598|2799->1615|3092->1880|3121->1881|3162->1894|3192->1896|3221->1897|3266->1914|3322->1942|3351->1943|3392->1956|3429->1965|3458->1966|3503->1983|3852->2304|3881->2305|3922->2318|3963->2331|3992->2332|4037->2349|4160->2444|4189->2445|4230->2458|4273->2473|4302->2474|4347->2491|5133->3249|5162->3250|5203->3263|5234->3266|5263->3267|5308->3284|5594->3542|5623->3543|5664->3556|5696->3560|5725->3561|5770->3578|6000->3780|6029->3781|6070->3794|6112->3808|6141->3809|6186->3826|6514->4126|6543->4127|6584->4140|6626->4154|6655->4155|6700->4172|6848->4292|6877->4293|6918->4306|6972->4332|7001->4333|7046->4350|7117->4393|7146->4394|7187->4407|7241->4433|7270->4434|7316->4451|7388->4494|7418->4495|7460->4508|7509->4528|7539->4529|7585->4546|7733->4665|7763->4666|7805->4679|7844->4689|7874->4690|7920->4707|7977->4735|8007->4736|8049->4749|8100->4771|8130->4772|8176->4789|8324->4908|8354->4909|8396->4919|8429->4924|8510->4977|8525->4982|8553->4988|8597->5004|8612->5009|8628->5015|8640->5031|8708->5089|8749->5091|8795->5108|8839->5124|8853->5128|8919->5172|8957->5204|8976->5213|9017->5215|9063->5232|9119->5260|9134->5265|9168->5277|9219->5306|9258->5317|9273->5322|9289->5328|9301->5344|9364->5397|9405->5399|9452->5418|9487->5443|9501->5447|9550->5457|9600->5478|9661->5511|9690->5530|9705->5535|9746->5537|9802->5565|9828->5569|9858->5570|9927->5619|9974->5627|10030->5655|10050->5665|10065->5670|10106->5672|10164->5702|10190->5706|10220->5708|10246->5712|10294->5740|10343->5750|10401->5779|10539->5889|10586->5914|10652->5951|10679->5955|10710->5957|10737->5961|10801->5993|10859->6019|10909->6040|11012->6115|11056->6149|11070->6153|11082->6185|11111->6204|11152->6206|11215->6241|11236->6252|11269->6275|11281->6315|11345->6369|11386->6371|11456->6412|11515->6443|11541->6447|11583->6461|11635->6490|11667->6493|11701->6516|11716->6520|11763->6527|11794->6528|11837->6542|11861->6543|11897->6546|11947->6567|11999->6596|12055->6623|12258->6803|12330->6893|12361->6914|12402->6916|12472->6957|12517->6974|12543->6978|12585->6992|12637->7021|12688->7043|12740->7072|12796->7099|12823->7103|12907->7189|12970->7246|13020->7267|13077->7292|13112->7321|13183->7382|13224->7384|13271->7402|13304->7407|13324->7417|13355->7426|13433->7476|13453->7486|13500->7523|13512->7551|13542->7571|13583->7573|13641->7602|13694->7627|13725->7636|13780->7663|13806->7667|13878->7729|13924->7746|13965->7781|14043->7849|14084->7851|14131->7869|14373->8083|14392->8092|14438->8115|14470->8118|14490->8127|14530->8144|14594->8180|14613->8189|14647->8213|14696->8223|14750->8248|14839->8309|14865->8313|14932->8348|14978->8365|15019->8400|15038->8409|15079->8411|15126->8436|15160->8442
                  LINES: 18->5|23->6|26->9|26->9|26->9|28->11|28->11|28->11|28->11|29->12|29->12|29->12|30->13|34->17|34->17|35->18|35->18|35->18|36->19|43->26|43->26|44->27|44->27|44->27|45->28|46->29|46->29|47->30|47->30|47->30|48->31|56->39|56->39|57->40|57->40|57->40|58->41|61->44|61->44|62->45|62->45|62->45|63->46|80->63|80->63|81->64|81->64|81->64|82->65|89->72|89->72|90->73|90->73|90->73|91->74|96->79|96->79|97->80|97->80|97->80|98->81|106->89|106->89|107->90|107->90|107->90|108->91|112->95|112->95|113->96|113->96|113->96|114->97|115->98|115->98|116->99|116->99|116->99|117->100|118->101|118->101|119->102|119->102|119->102|120->103|123->106|123->106|124->107|124->107|124->107|125->108|126->109|126->109|127->110|127->110|127->110|128->111|131->114|131->114|132->115|133->116|135->118|135->118|135->118|137->120|137->120|137->120|137->122|137->122|137->122|138->123|138->123|138->123|138->123|139->126|139->126|139->126|140->127|140->127|140->127|140->127|141->130|143->132|143->132|143->132|143->134|143->134|143->134|145->136|145->136|145->136|145->136|146->137|147->138|147->138|147->138|147->138|148->139|148->139|148->139|149->140|149->140|150->141|150->141|150->141|150->141|151->142|151->142|151->142|151->142|152->143|152->143|153->144|154->145|154->145|154->145|154->145|154->145|154->145|155->146|156->147|157->148|160->151|160->151|160->151|160->153|160->153|160->153|162->155|162->155|162->155|162->157|162->157|162->157|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|165->162|165->162|165->162|166->163|166->163|166->163|166->163|166->163|166->163|166->163|166->163|166->163|167->166|168->169|169->170|171->172|173->176|173->176|173->176|175->178|175->178|175->178|175->178|178->181|178->181|178->181|178->183|178->183|178->183|179->184|179->184|179->184|179->184|179->184|180->187|181->188|183->192|183->192|183->192|185->194|191->200|191->200|191->200|191->200|191->200|191->200|193->202|193->202|193->202|193->202|194->203|194->203|194->203|195->204|196->205|198->209|198->209|198->209|199->212|201->214
                  -- GENERATED --
              */
          