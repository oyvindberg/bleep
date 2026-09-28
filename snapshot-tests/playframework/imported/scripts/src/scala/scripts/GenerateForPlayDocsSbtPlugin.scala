
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForPlayDocsSbtPlugin extends BleepCodegenScript("GenerateForPlayDocsSbtPlugin") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set(s"""|play-docs-sbt-plugin""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|org/playframework/docs/sbtplugin/html/translationReport.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package org.playframework.docs.sbtplugin.html
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |
      |object translationReport extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template2[org.playframework.docs.sbtplugin.PlayDocsValidation.TranslationReport,String,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**/
      |  def apply/*1.2*/(report: org.playframework.docs.sbtplugin.PlayDocsValidation.TranslationReport, version: String):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*2.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <meta charset="utf-8">
      |        <meta http-equiv="X-UA-Compatible" content="IE=edge">
      |        <meta name="viewport" content="width=device-width, initial-scale=1">
      |
      |        <link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">
      |        <link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css">
      |        <script src="//code.jquery.com/jquery-2.2.0.min.js"></script>
      |        <script src="//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
      |        <title>Play Translation Report</title>
      |
      |        <style>
      |            ul ${"\"" * 3}),format.raw/*16.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*16.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}list-style-type: none;
      |            ${"\"" * 3}),format.raw/*18.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*18.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*19.13*/(${"\"" * 3}li.missing:before ${"\"" * 3}),format.raw/*19.31*/(${"\"" * 3}{${"\"" * 3}),format.raw/*19.32*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*20.17*/(${"\"" * 3}content: "-";
      |                position: relative;
      |                left: -5px;
      |                color: darkred;
      |            ${"\"" * 3}),format.raw/*24.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*24.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}li.missing ${"\"" * 3}),format.raw/*25.24*/(${"\"" * 3}{${"\"" * 3}),format.raw/*25.25*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*26.17*/(${"\"" * 3}text-indent: -5px;
      |                color: darkred;
      |            ${"\"" * 3}),format.raw/*28.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*28.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*29.13*/(${"\"" * 3}li.introduced:before ${"\"" * 3}),format.raw/*29.34*/(${"\"" * 3}{${"\"" * 3}),format.raw/*29.35*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*30.17*/(${"\"" * 3}content: "+";
      |                position: relative;
      |                left: -5px;
      |                color: darkorange;
      |            ${"\"" * 3}),format.raw/*34.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*34.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*35.13*/(${"\"" * 3}li.introduced ${"\"" * 3}),format.raw/*35.27*/(${"\"" * 3}{${"\"" * 3}),format.raw/*35.28*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*36.17*/(${"\"" * 3}text-indent: -5px;
      |                color: darkorange;
      |            ${"\"" * 3}),format.raw/*38.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*38.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*39.13*/(${"\"" * 3}li.ok ${"\"" * 3}),format.raw/*39.19*/(${"\"" * 3}{${"\"" * 3}),format.raw/*39.20*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*40.17*/(${"\"" * 3}color: darkgreen;
      |            ${"\"" * 3}),format.raw/*41.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*41.14*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*42.9*/(${"\"" * 3}</style>
      |    </head>
      |    <body>
      |        <div class="container">
      |            <h1>Play Translation Report</h1>
      |
      |            <h2>Play upstream documentation version: ${"\"" * 3}),_display_(/*48.55*/version),format.raw/*48.62*/(${"\"" * 3}</h2>
      |
      |            <a href="/@report?force">Rerun report</a>
      |
      |            <p>
      |                This report details the progress of this translation against the given version of the Play documentation,
      |                and attempts to identify potential issues.
      |            </p>
      |
      |            <p>Total parsed files: ${"\"" * 3}),_display_(/*57.37*/report/*57.43*/.total),format.raw/*57.49*/(${"\"" * 3}</p>
      |
      |            ${"\"" * 3}),_display_(if(report.missingFiles.nonEmpty)/*59.46*/ {_display_(Seq[Any](format.raw/*59.48*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*60.17*/(${"\"" * 3}<h3>Missing files</h3>
      |
      |                <p>
      |                    The following Markdown files are present in the official Play documentation, but are not present in the
      |                    translation.  This indicates that there may be translation work left to do.
      |                </p>
      |
      |                <ul>
      |                    ${"\"" * 3}),_display_(/*68.22*/for(file <- report.missingFiles) yield /*68.54*/ {_display_(Seq[Any](format.raw/*68.56*/(${"\"" * 3}
      |                        ${"\"" * 3}),format.raw/*69.25*/(${"\"" * 3}<li class="missing">${"\"" * 3}),_display_(/*69.46*/file),format.raw/*69.50*/(${"\"" * 3}</li>
      |                    ${"\"" * 3})))}),format.raw/*70.22*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*71.17*/(${"\"" * 3}</ul>
      |            ${"\"" * 3})))} else {null} ),format.raw/*72.14*/(${"\"" * 3}
      |
      |            ${"\"" * 3}),_display_(if(report.introducedFiles.nonEmpty)/*74.49*/ {_display_(Seq[Any](format.raw/*74.51*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*75.17*/(${"\"" * 3}<h3>Introduced files</h3>
      |
      |                <p>
      |                    The following Markdown files are not present in the official Play documentation, but are present in the
      |                    translation.  This could indicate many things, such as documentation being restructured, or a typo in
      |                    the file name.
      |                </p>
      |
      |                <ul>
      |                ${"\"" * 3}),_display_(/*84.18*/for(file <- report.introducedFiles) yield /*84.53*/ {_display_(Seq[Any](format.raw/*84.55*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*85.21*/(${"\"" * 3}<li class="introduced">${"\"" * 3}),_display_(/*85.45*/file),format.raw/*85.49*/(${"\"" * 3}</li>
      |                ${"\"" * 3})))}),format.raw/*86.18*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*87.17*/(${"\"" * 3}</ul>
      |            ${"\"" * 3})))} else {null} ),format.raw/*88.14*/(${"\"" * 3}
      |
      |            ${"\"" * 3}),_display_(if(report.changedPathFiles.nonEmpty)/*90.50*/ {_display_(Seq[Any](format.raw/*90.52*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*91.17*/(${"\"" * 3}<h3>Non matching paths</h3>
      |
      |                <p>
      |                    The following Markdown files have changed paths.  This could create issues, particularly with sourcing
      |                    code samples.
      |                </p>
      |
      |                <dl>
      |                ${"\"" * 3}),_display_(/*99.18*/for(file <- report.changedPathFiles) yield /*99.54*/ {_display_(Seq[Any](format.raw/*99.56*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*100.21*/(${"\"" * 3}<dt>${"\"" * 3}),_display_(/*100.26*/file/*100.30*/._2),format.raw/*100.33*/(${"\"" * 3}</dt>
      |                    <dd>-> ${"\"" * 3}),_display_(/*101.29*/file/*101.33*/._1),format.raw/*101.36*/(${"\"" * 3}</dd>
      |                ${"\"" * 3})))}),format.raw/*102.18*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*103.17*/(${"\"" * 3}</dl>
      |            ${"\"" * 3})))} else {null} ),format.raw/*104.14*/(${"\"" * 3}
      |
      |            ${"\"" * 3}),_display_(if(report.codeSampleIssues.nonEmpty)/*106.50*/ {_display_(Seq[Any](format.raw/*106.52*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*107.17*/(${"\"" * 3}<h3>Code sample issues</h3>
      |
      |                <p>
      |                    The following Markdown files have potential issues in code samples.  They are either missing code samples,
      |                    or they refer to code samples that the official documentation doesn't.  This could indicate an error
      |                    in translating, or that something has changed.
      |                </p>
      |
      |                <dl>
      |                    ${"\"" * 3}),_display_(/*116.22*/for(file <- report.codeSampleIssues) yield /*116.58*/ {_display_(Seq[Any](format.raw/*116.60*/(${"\"" * 3}
      |                        ${"\"" * 3}),format.raw/*117.25*/(${"\"" * 3}<dt>${"\"" * 3}),_display_(/*117.30*/file/*117.34*/.name),format.raw/*117.39*/(${"\"" * 3}</dt>
      |                        <dd>
      |                            <p>Total code samples in official documentation: ${"\"" * 3}),_display_(/*119.79*/file/*119.83*/.totalCodeSamples),format.raw/*119.100*/(${"\"" * 3}</p>
      |
      |                            <ul>
      |                                ${"\"" * 3}),_display_(/*122.34*/for(codeSample <- file.missingCodeSamples) yield /*122.76*/ {_display_(Seq[Any](format.raw/*122.78*/(${"\"" * 3}
      |                                    ${"\"" * 3}),format.raw/*123.37*/(${"\"" * 3}<li class="missing">[${"\"" * 3}),_display_(/*123.59*/codeSample/*123.69*/.segment),format.raw/*123.77*/(${"\"" * 3}](${"\"" * 3}),_display_(/*123.80*/codeSample/*123.90*/.source),format.raw/*123.97*/(${"\"" * 3})</li>
      |                                ${"\"" * 3})))}),format.raw/*124.34*/(${"\"" * 3}
      |                                ${"\"" * 3}),_display_(/*125.34*/for(codeSample <- file.introducedCodeSamples) yield /*125.79*/ {_display_(Seq[Any](format.raw/*125.81*/(${"\"" * 3}
      |                                    ${"\"" * 3}),format.raw/*126.37*/(${"\"" * 3}<li class="introduced">[${"\"" * 3}),_display_(/*126.62*/codeSample/*126.72*/.segment),format.raw/*126.80*/(${"\"" * 3}](${"\"" * 3}),_display_(/*126.83*/codeSample/*126.93*/.source),format.raw/*126.100*/(${"\"" * 3})</li>
      |                                ${"\"" * 3})))}),format.raw/*127.34*/(${"\"" * 3}
      |                            ${"\"" * 3}),format.raw/*128.29*/(${"\"" * 3}</ul>
      |                        </dd>
      |                    ${"\"" * 3})))}),format.raw/*130.22*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*131.17*/(${"\"" * 3}</dl>
      |            ${"\"" * 3})))} else {null} ),format.raw/*132.14*/(${"\"" * 3}
      |
      |            ${"\"" * 3}),_display_(if(report.okFiles.nonEmpty)/*134.41*/ {_display_(Seq[Any](format.raw/*134.43*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*135.17*/(${"\"" * 3}<h3>Ok files</h3>
      |
      |                <p>
      |                    The following files have no problems.
      |                </p>
      |
      |                <ul>
      |                    ${"\"" * 3}),_display_(/*142.22*/for(file <- report.okFiles) yield /*142.49*/ {_display_(Seq[Any](format.raw/*142.51*/(${"\"" * 3}
      |                        ${"\"" * 3}),format.raw/*143.25*/(${"\"" * 3}<li class="ok">${"\"" * 3}),_display_(/*143.41*/file),format.raw/*143.45*/(${"\"" * 3}</li>
      |                    ${"\"" * 3})))}),format.raw/*144.22*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*145.17*/(${"\"" * 3}</ul>
      |            ${"\"" * 3})))} else {null} ),format.raw/*146.14*/(${"\"" * 3}
      |
      |        ${"\"" * 3}),format.raw/*148.9*/(${"\"" * 3}</div>
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(report:org.playframework.docs.sbtplugin.PlayDocsValidation.TranslationReport,version:String): play.twirl.api.HtmlFormat.Appendable = apply(report,version)
      |
      |  def f:((org.playframework.docs.sbtplugin.PlayDocsValidation.TranslationReport,String) => play.twirl.api.HtmlFormat.Appendable) = (report,version) => apply(report,version)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: dev-mode/play-docs-sbt-plugin/src/main/twirl/org/playframework/docs/sbtplugin/translationReport.scala.html
      |                  HASH: a7e38f81124cd456d59f3f7bc3959f694db19fe6
      |                  MATRIX: 675->1|865->98|1563->768|1592->769|1637->786|1700->821|1729->822|1770->835|1816->853|1845->854|1890->871|2040->993|2069->994|2110->1007|2149->1018|2178->1019|2223->1036|2314->1099|2343->1100|2384->1113|2433->1134|2462->1135|2507->1152|2660->1277|2689->1278|2730->1291|2772->1305|2801->1306|2846->1323|2940->1389|2969->1390|3010->1403|3044->1409|3073->1410|3118->1427|3176->1457|3205->1458|3241->1467|3432->1631|3460->1638|3800->1952|3815->1958|3842->1964|3920->2015|3960->2017|4005->2034|4360->2362|4408->2394|4448->2396|4501->2421|4549->2442|4574->2446|4632->2473|4677->2490|4740->2509|4817->2559|4857->2561|4902->2578|5317->2966|5368->3001|5408->3003|5457->3024|5508->3048|5533->3052|5587->3075|5632->3092|5695->3111|5773->3162|5813->3164|5858->3181|6151->3447|6203->3483|6243->3485|6293->3506|6326->3511|6340->3515|6365->3518|6427->3552|6441->3556|6466->3559|6521->3582|6567->3599|6631->3618|6710->3669|6751->3671|6797->3688|7253->4116|7306->4152|7347->4154|7401->4179|7434->4184|7448->4188|7475->4193|7616->4306|7630->4310|7670->4327|7770->4399|7829->4441|7870->4443|7936->4480|7986->4502|8006->4512|8036->4520|8067->4523|8087->4533|8116->4540|8188->4580|8250->4614|8312->4659|8353->4661|8419->4698|8472->4723|8492->4733|8522->4741|8553->4744|8573->4754|8603->4761|8675->4801|8733->4830|8822->4887|8868->4904|8932->4923|9002->4965|9043->4967|9089->4984|9278->5145|9322->5172|9363->5174|9417->5199|9461->5215|9487->5219|9546->5246|9592->5263|9656->5282|9694->5292
      |                  LINES: 14->1|19->2|33->16|33->16|34->17|35->18|35->18|36->19|36->19|36->19|37->20|41->24|41->24|42->25|42->25|42->25|43->26|45->28|45->28|46->29|46->29|46->29|47->30|51->34|51->34|52->35|52->35|52->35|53->36|55->38|55->38|56->39|56->39|56->39|57->40|58->41|58->41|59->42|65->48|65->48|74->57|74->57|74->57|76->59|76->59|77->60|85->68|85->68|85->68|86->69|86->69|86->69|87->70|88->71|89->72|91->74|91->74|92->75|101->84|101->84|101->84|102->85|102->85|102->85|103->86|104->87|105->88|107->90|107->90|108->91|116->99|116->99|116->99|117->100|117->100|117->100|117->100|118->101|118->101|118->101|119->102|120->103|121->104|123->106|123->106|124->107|133->116|133->116|133->116|134->117|134->117|134->117|134->117|136->119|136->119|136->119|139->122|139->122|139->122|140->123|140->123|140->123|140->123|140->123|140->123|140->123|141->124|142->125|142->125|142->125|143->126|143->126|143->126|143->126|143->126|143->126|143->126|144->127|145->128|147->130|148->131|149->132|151->134|151->134|152->135|159->142|159->142|159->142|160->143|160->143|160->143|161->144|162->145|163->146|165->148
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play-docs-sbt-plugin""".stripMargin).contains(target.project.value)) {
        val to = target.resources.resolve(s"""|sbt/sbt.autoplugins""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|org.playframework.docs.sbtplugin.PlayDocsPlugin
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}