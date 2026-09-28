
package views.html.defaultpages

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
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
                  HASH: adb6c0b09b3a79b58dc45a5bb8b9ce6476214882
                  MATRIX: 858->146|1059->254|1145->314|1158->319|1184->325|2210->1324|2224->1329|2281->1377|2321->1379|2362->1392|2406->1408|2435->1409|2480->1426|2662->1580|2691->1581|2732->1594|2763->1597|2792->1598|2837->1615|3130->1880|3159->1881|3200->1894|3230->1896|3259->1897|3304->1914|3360->1942|3389->1943|3430->1956|3467->1965|3496->1966|3541->1983|3890->2304|3919->2305|3960->2318|4001->2331|4030->2332|4075->2349|4198->2444|4227->2445|4268->2458|4311->2473|4340->2474|4385->2491|5171->3249|5200->3250|5241->3263|5272->3266|5301->3267|5346->3284|5632->3542|5661->3543|5702->3556|5734->3560|5763->3561|5808->3578|6038->3780|6067->3781|6108->3794|6150->3808|6179->3809|6224->3826|6552->4126|6581->4127|6622->4140|6664->4154|6693->4155|6738->4172|6886->4292|6915->4293|6956->4306|7010->4332|7039->4333|7084->4350|7155->4393|7184->4394|7225->4407|7279->4433|7308->4434|7354->4451|7426->4494|7456->4495|7498->4508|7547->4528|7577->4529|7623->4546|7771->4665|7801->4666|7843->4679|7882->4689|7912->4690|7958->4707|8015->4735|8045->4736|8087->4749|8138->4771|8168->4772|8214->4789|8362->4908|8392->4909|8434->4919|8467->4924|8548->4977|8563->4982|8591->4988|8635->5004|8650->5009|8666->5015|8678->5031|8746->5089|8787->5091|8833->5108|8877->5124|8891->5128|8957->5172|8995->5204|9014->5213|9055->5215|9101->5232|9157->5260|9172->5265|9206->5277|9257->5306|9296->5317|9311->5322|9327->5328|9339->5344|9402->5397|9443->5399|9490->5418|9525->5443|9539->5447|9588->5457|9638->5478|9699->5511|9728->5530|9743->5535|9784->5537|9840->5565|9866->5569|9896->5570|9965->5619|10012->5627|10068->5655|10088->5665|10103->5670|10144->5672|10202->5702|10228->5706|10258->5708|10284->5712|10332->5740|10381->5750|10439->5779|10577->5889|10624->5914|10690->5951|10717->5955|10748->5957|10775->5961|10839->5993|10897->6019|10947->6040|11050->6115|11094->6149|11108->6153|11120->6185|11149->6204|11190->6206|11253->6241|11274->6252|11307->6275|11319->6315|11383->6369|11424->6371|11494->6412|11553->6443|11579->6447|11621->6461|11673->6490|11705->6493|11739->6516|11754->6520|11801->6527|11832->6528|11875->6542|11899->6543|11935->6546|11985->6567|12037->6596|12093->6623|12296->6803|12368->6893|12399->6914|12440->6916|12510->6957|12555->6974|12581->6978|12623->6992|12675->7021|12726->7043|12778->7072|12834->7099|12861->7103|12945->7189|13008->7246|13058->7267|13115->7292|13150->7321|13221->7382|13262->7384|13309->7402|13342->7407|13362->7417|13393->7426|13471->7476|13491->7486|13538->7523|13550->7551|13580->7571|13621->7573|13679->7602|13732->7627|13763->7636|13818->7663|13844->7667|13916->7729|13962->7746|14003->7781|14081->7849|14122->7851|14169->7869|14411->8083|14430->8092|14476->8115|14508->8118|14528->8127|14568->8144|14632->8180|14651->8189|14685->8213|14734->8223|14788->8248|14877->8309|14903->8313|14970->8348|15016->8365|15057->8400|15076->8409|15117->8411|15164->8436|15198->8442
                  LINES: 19->5|24->6|27->9|27->9|27->9|29->11|29->11|29->11|29->11|30->12|30->12|30->12|31->13|35->17|35->17|36->18|36->18|36->18|37->19|44->26|44->26|45->27|45->27|45->27|46->28|47->29|47->29|48->30|48->30|48->30|49->31|57->39|57->39|58->40|58->40|58->40|59->41|62->44|62->44|63->45|63->45|63->45|64->46|81->63|81->63|82->64|82->64|82->64|83->65|90->72|90->72|91->73|91->73|91->73|92->74|97->79|97->79|98->80|98->80|98->80|99->81|107->89|107->89|108->90|108->90|108->90|109->91|113->95|113->95|114->96|114->96|114->96|115->97|116->98|116->98|117->99|117->99|117->99|118->100|119->101|119->101|120->102|120->102|120->102|121->103|124->106|124->106|125->107|125->107|125->107|126->108|127->109|127->109|128->110|128->110|128->110|129->111|132->114|132->114|133->115|134->116|136->118|136->118|136->118|138->120|138->120|138->120|138->122|138->122|138->122|139->123|139->123|139->123|139->123|140->126|140->126|140->126|141->127|141->127|141->127|141->127|142->130|144->132|144->132|144->132|144->134|144->134|144->134|146->136|146->136|146->136|146->136|147->137|148->138|148->138|148->138|148->138|149->139|149->139|149->139|150->140|150->140|151->141|151->141|151->141|151->141|152->142|152->142|152->142|152->142|153->143|153->143|154->144|155->145|155->145|155->145|155->145|155->145|155->145|156->146|157->147|158->148|161->151|161->151|161->151|161->153|161->153|161->153|163->155|163->155|163->155|163->157|163->157|163->157|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|166->162|166->162|166->162|167->163|167->163|167->163|167->163|167->163|167->163|167->163|167->163|167->163|168->166|169->169|170->170|172->172|174->176|174->176|174->176|176->178|176->178|176->178|176->178|179->181|179->181|179->181|179->183|179->183|179->183|180->184|180->184|180->184|180->184|180->184|181->187|182->188|184->192|184->192|184->192|186->194|192->200|192->200|192->200|192->200|192->200|192->200|194->202|194->202|194->202|194->202|195->203|195->203|195->203|196->204|197->205|199->209|199->209|199->209|200->212|202->214
                  -- GENERATED --
              */
          