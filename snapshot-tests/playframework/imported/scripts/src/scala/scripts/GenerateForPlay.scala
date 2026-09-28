
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForPlay extends BleepCodegenScript("GenerateForPlay") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|PlayVersion.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package play.core
      |
      |object PlayVersion {
      |  val current = "3.1.0-M3-SNAPSHOT"
      |  val scalaVersion = "3.3.6"
      |  val sbtVersion = "1.11.4"
      |  val pekkoVersion = "1.0.3"
      |  val pekkoHttpVersion = "1.0.1"
      |}
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|PlayVersion.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package play.core
      |
      |object PlayVersion {
      |  val current = "3.1.0-M3-SNAPSHOT"
      |  val scalaVersion = "2.13.16"
      |  val sbtVersion = "1.11.4"
      |  val pekkoVersion = "1.0.3"
      |  val pekkoHttpVersion = "1.0.1"
      |}
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/badRequest.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object badRequest extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[String,String,String,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 400 Bad Request responses.
      | */
      |  def apply/*4.2*/(method: String, uri: String, error:String)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Bad Request</title>
      |        ${"\"" * 3}),_display_(/*9.10*/views/*9.15*/.html.helper.style(Symbol("type") -> "text/css")/*9.63*/ {_display_(Seq[Any](format.raw/*9.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*10.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*10.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*10.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*11.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*15.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*15.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*16.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*16.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}margin: 0;
      |                background: #AD632A;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #9F5805;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*24.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*24.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*25.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*25.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*26.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F6A960;
      |                border-top: 4px solid #D29052;
      |                color: #733512;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7F5B;
      |            ${"\"" * 3}),format.raw/*34.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*34.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*35.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*36.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Bad Request</h1>
      |
      |        <p id="detail">
      |            For request '${"\"" * 3}),_display_(/*41.27*/method),format.raw/*41.33*/(${"\"" * 3} ${"\"" * 3}),_display_(/*41.35*/uri),format.raw/*41.38*/(${"\"" * 3}' [${"\"" * 3}),_display_(/*41.42*/error),format.raw/*41.47*/(${"\"" * 3}]
      |        </p>
      |
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(method:String,uri:String,error:String,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(method,uri,error)(request)
      |
      |  def f:((String,String,String) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (method,uri,error) => (request) => apply(method,uri,error)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/badRequest.scala.html
      |                  HASH: a6ac94fcac0527a8eacf73c0b278f7e522b859a0
      |                  MATRIX: 751->56|934->146|1048->234|1061->239|1117->287|1156->289|1197->302|1241->318|1270->319|1315->336|1497->490|1526->491|1567->504|1598->507|1627->508|1672->525|1965->790|1994->791|2035->804|2072->813|2101->814|2146->831|2495->1152|2524->1153|2565->1163|2597->1168|2723->1267|2750->1273|2779->1275|2803->1278|2834->1282|2860->1287
      |                  LINES: 18->4|23->5|27->9|27->9|27->9|27->9|28->10|28->10|28->10|29->11|33->15|33->15|34->16|34->16|34->16|35->17|42->24|42->24|43->25|43->25|43->25|44->26|52->34|52->34|53->35|54->36|59->41|59->41|59->41|59->41|59->41|59->41
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/badRequest.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object badRequest extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[String,String,String,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 400 Bad Request responses.
      | */
      |  def apply/*4.2*/(method: String, uri: String, error:String)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Bad Request</title>
      |        ${"\"" * 3}),_display_(/*9.10*/views/*9.15*/.html.helper.style(Symbol("type") -> "text/css")/*9.63*/ {_display_(Seq[Any](format.raw/*9.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*10.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*10.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*10.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*11.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*15.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*15.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*16.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*16.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}margin: 0;
      |                background: #AD632A;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #9F5805;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*24.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*24.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*25.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*25.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*26.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F6A960;
      |                border-top: 4px solid #D29052;
      |                color: #733512;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7F5B;
      |            ${"\"" * 3}),format.raw/*34.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*34.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*35.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*36.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Bad Request</h1>
      |
      |        <p id="detail">
      |            For request '${"\"" * 3}),_display_(/*41.27*/method),format.raw/*41.33*/(${"\"" * 3} ${"\"" * 3}),_display_(/*41.35*/uri),format.raw/*41.38*/(${"\"" * 3}' [${"\"" * 3}),_display_(/*41.42*/error),format.raw/*41.47*/(${"\"" * 3}]
      |        </p>
      |
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(method:String,uri:String,error:String,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(method,uri,error)(request)
      |
      |  def f:((String,String,String) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (method,uri,error) => (request) => apply(method,uri,error)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/badRequest.scala.html
      |                  HASH: 848647593c0d741eba3309cb5c95a902446845db
      |                  MATRIX: 713->56|896->146|1010->234|1023->239|1079->287|1118->289|1159->302|1203->318|1232->319|1277->336|1459->490|1488->491|1529->504|1560->507|1589->508|1634->525|1927->790|1956->791|1997->804|2034->813|2063->814|2108->831|2457->1152|2486->1153|2527->1163|2559->1168|2685->1267|2712->1273|2741->1275|2765->1278|2796->1282|2822->1287
      |                  LINES: 17->4|22->5|26->9|26->9|26->9|26->9|27->10|27->10|27->10|28->11|32->15|32->15|33->16|33->16|33->16|34->17|41->24|41->24|42->25|42->25|42->25|43->26|51->34|51->34|52->35|53->36|58->41|58->41|58->41|58->41|58->41|58->41
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/devError.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object devError extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[Option[String],play.api.UsefulException,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 500 Internal Server Error responses, in development mode.
      | * This page display the error in the source code context.
      | */
      |  def apply/*5.2*/(playEditor: Option[String], error: play.api.UsefulException)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>${"\"" * 3}),_display_(/*9.17*/error/*9.22*/.title),format.raw/*9.28*/(${"\"" * 3}</title>
      |        <link rel="shortcut icon" href="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAlFJREFUeNqUU8tOFEEUPVVdNV3dPe8xYRBnjGhmBgKjKzCIiQvBoIaNbly5Z+PSv3Aj7DSiP2B0rwkLGVdGgxITSCRIJGSMEQWZR3eVt5sEFBgTb/dN1yvnnHtPNTPG4PqdHgCMXnPRSZrpSuH8vUJu4DE4rYHDGAZDX62BZttHqTiIayM3gGiXQsgYLEvATaqxU+dy1U13YXapXptpNHY8iwn8KyIAzm1KBdtRZWErpI5lEWTXp5Z/vHpZ3/wyKKwYGGOdAYwR0EZwoezTYApBEIObyELl/aE1/83cp40Pt5mxqCKrE4Ck+mVWKKcI5tA8BLEhRBKJLjez6a7MLq7XZtp+yyOawwCBtkiBVZDKzRk4NN7NQBMYPHiZDFhXY+p9ff7F961vVcnl4R5I2ykJ5XFN7Ab7Gc61VoipNBKF+PDyztu5lfrSLT/wIwCxq0CAGtXHZTzqR2jtwQiXONma6hHpj9sLT7YaPxfTXuZdBGA02Wi7FS48YiTfj+i2NhqtdhP5RC8mh2/Op7y0v6eAcWVLFT8D7kWX5S9mepp+C450MV6aWL1cGnvkxbwHtLW2B9AOkLeUd9KEDuh9fl/7CEj7YH5g+3r/lWfF9In7tPz6T4IIwBJOr1SJyIGQMZQbsh5P9uBq5VJtqHh2mo49pdw5WFoEwKWqWHacaWOjQXWGcifKo6vj5RGS6zykI587XeUIQDqJSmAp+lE4qt19W5P9o8+Lma5DcjsC8JiT607lMVkdqQ0Vyh3lHhmh52tfNy78ajXv0rgYzv8nfwswANuk+7sD/Q0aAAAAAElFTkSuQmCC">
      |        ${"\"" * 3}),_display_(/*11.10*/views/*11.15*/.html.helper.style(Symbol("type") -> "text/css")/*11.63*/ {_display_(Seq[Any](format.raw/*11.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*12.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*12.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*12.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*13.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*17.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*18.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*18.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*19.17*/(${"\"" * 3}margin: 0;
      |                background: #A31012;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #690000;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*26.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*27.13*/(${"\"" * 3}a ${"\"" * 3}),format.raw/*27.15*/(${"\"" * 3}{${"\"" * 3}),format.raw/*27.16*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*28.17*/(${"\"" * 3}color: #D36D6D;
      |            ${"\"" * 3}),format.raw/*29.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*29.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*30.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*30.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*30.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*31.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F5A0A0;
      |                border-top: 4px solid #D36D6D;
      |                color: #730000;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7A7A;
      |            ${"\"" * 3}),format.raw/*39.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*39.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*40.13*/(${"\"" * 3}p#detail.pre ${"\"" * 3}),format.raw/*40.26*/(${"\"" * 3}{${"\"" * 3}),format.raw/*40.27*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*41.17*/(${"\"" * 3}white-space: pre;
      |                font-size: 13px;
      |                overflow: auto;
      |            ${"\"" * 3}),format.raw/*44.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*44.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*45.13*/(${"\"" * 3}p#detail input ${"\"" * 3}),format.raw/*45.28*/(${"\"" * 3}{${"\"" * 3}),format.raw/*45.29*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*46.17*/(${"\"" * 3}background: #AE1113;
      |                background: -webkit-linear-gradient(#AE1113, #A31012);
      |                background: -o-linear-gradient(#AE1113, #A31012);
      |                background: -moz-linear-gradient(#AE1113, #A31012);
      |                background: linear-gradient(#AE1113, #A31012);
      |                border: 1px solid #790000;
      |                padding: 3px 10px;
      |                text-shadow: 1px 1px 0 rgba(0, 0, 0, .5);
      |                color: white;
      |                border-radius: 3px;
      |                cursor: pointer;
      |                font-family: Monaco, 'Lucida Console';
      |                font-size: 12px;
      |                margin: 0 10px;
      |                display: inline-block;
      |                position: relative;
      |                top: -1px;
      |            ${"\"" * 3}),format.raw/*63.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*63.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*64.13*/(${"\"" * 3}h2 ${"\"" * 3}),format.raw/*64.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*64.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*65.17*/(${"\"" * 3}margin: 0;
      |                padding: 5px 45px;
      |                font-size: 12px;
      |                background: #333;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-top: 4px solid #2a2a2a;
      |            ${"\"" * 3}),format.raw/*72.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*72.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*73.13*/(${"\"" * 3}pre ${"\"" * 3}),format.raw/*73.17*/(${"\"" * 3}{${"\"" * 3}),format.raw/*73.18*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*74.17*/(${"\"" * 3}margin: 0;
      |                border-bottom: 1px solid #DDD;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
      |                position: relative;
      |                font-size: 12px;
      |            ${"\"" * 3}),format.raw/*79.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*79.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*80.13*/(${"\"" * 3}pre span.line ${"\"" * 3}),format.raw/*80.27*/(${"\"" * 3}{${"\"" * 3}),format.raw/*80.28*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*81.17*/(${"\"" * 3}text-align: right;
      |                display: inline-block;
      |                padding: 5px 5px;
      |                width: 30px;
      |                background: #D6D6D6;
      |                color: #8B8B8B;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
      |                font-weight: bold;
      |            ${"\"" * 3}),format.raw/*89.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*89.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*90.13*/(${"\"" * 3}pre span.code ${"\"" * 3}),format.raw/*90.27*/(${"\"" * 3}{${"\"" * 3}),format.raw/*90.28*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*91.17*/(${"\"" * 3}padding: 5px 5px;
      |                position: absolute;
      |                right: 0;
      |                left: 40px;
      |            ${"\"" * 3}),format.raw/*95.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*95.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*96.13*/(${"\"" * 3}pre:first-child span.code ${"\"" * 3}),format.raw/*96.39*/(${"\"" * 3}{${"\"" * 3}),format.raw/*96.40*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*97.17*/(${"\"" * 3}border-top: 4px solid #CDCDCD;
      |            ${"\"" * 3}),format.raw/*98.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*98.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*99.13*/(${"\"" * 3}pre:first-child span.line ${"\"" * 3}),format.raw/*99.39*/(${"\"" * 3}{${"\"" * 3}),format.raw/*99.40*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*100.17*/(${"\"" * 3}border-top: 4px solid #B6B6B6;
      |            ${"\"" * 3}),format.raw/*101.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*101.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*102.13*/(${"\"" * 3}pre.error span.line ${"\"" * 3}),format.raw/*102.33*/(${"\"" * 3}{${"\"" * 3}),format.raw/*102.34*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*103.17*/(${"\"" * 3}background: #A31012;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |            ${"\"" * 3}),format.raw/*106.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*106.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*107.13*/(${"\"" * 3}pre.error ${"\"" * 3}),format.raw/*107.23*/(${"\"" * 3}{${"\"" * 3}),format.raw/*107.24*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*108.17*/(${"\"" * 3}color: #A31012;
      |            ${"\"" * 3}),format.raw/*109.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*109.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*110.13*/(${"\"" * 3}pre.error span.marker ${"\"" * 3}),format.raw/*110.35*/(${"\"" * 3}{${"\"" * 3}),format.raw/*110.36*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*111.17*/(${"\"" * 3}background: #A31012;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |            ${"\"" * 3}),format.raw/*114.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*114.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*115.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*116.5*/(${"\"" * 3}</head>
      |    <body id="play-error-page">
      |        <h1>${"\"" * 3}),_display_(/*118.14*/error/*118.19*/.title),format.raw/*118.25*/(${"\"" * 3}</h1>
      |
      |        ${"\"" * 3}),_display_(/*120.10*/error/*120.15*/ match/*120.21*/ {/*122.13*/case description:play.api.PlayException.RichDescription =>/*122.71*/ {_display_(Seq[Any](format.raw/*122.73*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*123.17*/(${"\"" * 3}<p id="detail">${"\"" * 3}),_display_(/*123.33*/play/*123.37*/.twirl.api.Html(description.htmlDescription)),format.raw/*123.81*/(${"\"" * 3}</p>
      |            ${"\"" * 3})))}/*126.13*/case _ =>/*126.22*/ {_display_(Seq[Any](format.raw/*126.24*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*127.17*/(${"\"" * 3}<p id="detail" class="pre">${"\"" * 3}),_display_(/*127.45*/error/*127.50*/.description),format.raw/*127.62*/(${"\"" * 3}</p>
      |            ${"\"" * 3})))}}),format.raw/*130.10*/(${"\"" * 3}
      |
      |        ${"\"" * 3}),_display_(/*132.10*/error/*132.15*/ match/*132.21*/ {/*134.13*/case source:play.api.PlayException.ExceptionSource =>/*134.66*/ {_display_(Seq[Any](format.raw/*134.68*/(${"\"" * 3}
      |
      |                ${"\"" * 3}),_display_(/*136.18*/Option(source.sourceName)/*136.43*/.map/*136.47*/ { name =>_display_(Seq[Any](format.raw/*136.57*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*137.21*/(${"\"" * 3}<h2>
      |                        In ${"\"" * 3}),_display_(/*138.29*/Option(source.line)/*138.48*/.fold/*138.53*/ {_display_(Seq[Any](format.raw/*138.55*/(${"\"" * 3}
      |                          ${"\"" * 3}),_display_(/*139.28*/name),format.raw/*139.32*/(${"\"" * 3} ${"\"" * 3}),format.raw/*139.33*/(${"\"" * 3}(line number not found)
      |                        ${"\"" * 3})))}/*140.26*/{line =>_display_(Seq[Any](format.raw/*140.34*/(${"\"" * 3}
      |                          ${"\"" * 3}),_display_(/*141.28*/playEditor/*141.38*/.fold/*141.43*/ {_display_(Seq[Any](format.raw/*141.45*/(${"\"" * 3}
      |                            ${"\"" * 3}),_display_(/*142.30*/name),format.raw/*142.34*/(${"\"" * 3}:${"\"" * 3}),_display_(/*142.36*/line),format.raw/*142.40*/(${"\"" * 3}
      |                          ${"\"" * 3})))}/*143.28*/ { link =>_display_(Seq[Any](format.raw/*143.38*/(${"\"" * 3}
      |                            ${"\"" * 3}),format.raw/*144.29*/(${"\"" * 3}<iframe name="_onlyForFiringEditorLink" style="display:none;"></iframe>
      |                            <a href=${"\"" * 3}"),_display_(/*145.39*/{link.format(name, line)}),format.raw/*145.64*/(${"\"" * 3}" target="_onlyForFiringEditorLink">${"\"" * 3}),_display_(/*145.101*/name),format.raw/*145.105*/(${"\"" * 3}:${"\"" * 3}),_display_(/*145.107*/line),format.raw/*145.111*/(${"\"" * 3}</a>
      |                          ${"\"" * 3})))}),format.raw/*146.28*/(${"\"" * 3}
      |                        ${"\"" * 3})))}),format.raw/*147.26*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*148.21*/(${"\"" * 3}</h2>
      |
      |                    <div id="source-code">
      |                        ${"\"" * 3}),_display_(/*151.26*/Option(source.interestingLines(4))/*151.60*/.map/*151.64*/ {/*153.29*/case interesting =>/*153.48*/ {_display_(Seq[Any](format.raw/*153.50*/(${"\"" * 3}
      |
      |                                ${"\"" * 3}),_display_(/*155.34*/interesting/*155.45*/.focus.zipWithIndex.map/*155.68*/ {/*157.37*/case (line,index) if index == interesting.errorLine =>/*157.91*/ {_display_(Seq[Any](format.raw/*157.93*/(${"\"" * 3}
      |                                        ${"\"" * 3}),format.raw/*158.41*/(${"\"" * 3}<pre class="error" data-file=${"\"" * 3}"),_display_(/*158.72*/name),format.raw/*158.76*/(${"\"" * 3}" data-line=${"\"" * 3}"),_display_(/*158.90*/(interesting.firstLine+index)),format.raw/*158.119*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*158.122*/Option(source.position)/*158.145*/.map/*158.149*/ { c =>_display_(Seq[Any](format.raw/*158.156*/(${"\"" * 3} ${"\"" * 3}),format.raw/*158.157*/(${"\"" * 3}data-column=${"\"" * 3}"),_display_(/*158.171*/c),format.raw/*158.172*/(${"\"" * 3}" ${"\"" * 3})))}),format.raw/*158.175*/(${"\"" * 3}><span class="line">${"\"" * 3}),_display_(/*158.196*/(interesting.firstLine+index)),format.raw/*158.225*/(${"\"" * 3}</span><span class="code">${"\"" * 3}),_display_(/*158.252*/(Option(source.position).map(pos => (line+" ").zipWithIndex.map{ case (c,i) if i == pos => Html(${"\"" * 3}<span class="marker">${"\"" * 3} + c + ${"\"" * 3}</span>${"\"" * 3}); case (c,_) => c}).getOrElse(line))),format.raw/*158.432*/(${"\"" * 3}</span></pre>
      |
      |                                    ${"\"" * 3})))}/*162.37*/case (line, index) =>/*162.58*/ {_display_(Seq[Any](format.raw/*162.60*/(${"\"" * 3}
      |                                        ${"\"" * 3}),format.raw/*163.41*/(${"\"" * 3}<pre data-file=${"\"" * 3}"),_display_(/*163.58*/name),format.raw/*163.62*/(${"\"" * 3}" data-line=${"\"" * 3}"),_display_(/*163.76*/(interesting.firstLine+index)),format.raw/*163.105*/(${"\"" * 3}"><span class="line">${"\"" * 3}),_display_(/*163.127*/(interesting.firstLine+index)),format.raw/*163.156*/(${"\"" * 3}</span><span class="code">${"\"" * 3}),_display_(/*163.183*/line),format.raw/*163.187*/(${"\"" * 3}</span></pre>
      |                                    ${"\"" * 3})))}}),format.raw/*166.34*/(${"\"" * 3}
      |                            ${"\"" * 3})))}}),format.raw/*169.26*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*170.21*/(${"\"" * 3}</div>
      |
      |                ${"\"" * 3})))}),format.raw/*172.18*/(${"\"" * 3}
      |
      |            ${"\"" * 3})))}/*176.13*/case attachment:play.api.PlayException.ExceptionAttachment =>/*176.74*/ {_display_(Seq[Any](format.raw/*176.76*/(${"\"" * 3}
      |
      |                ${"\"" * 3}),format.raw/*178.17*/(${"\"" * 3}<h2>${"\"" * 3}),_display_(/*178.22*/attachment/*178.32*/.subTitle),format.raw/*178.41*/(${"\"" * 3}</h2>
      |
      |                <div>
      |                    ${"\"" * 3}),_display_(/*181.22*/attachment/*181.32*/.content.split("\\n").zipWithIndex.map/*181.69*/ {/*183.25*/case (line,index) =>/*183.45*/ {_display_(Seq[Any](format.raw/*183.47*/(${"\"" * 3}
      |                            ${"\"" * 3}),format.raw/*184.29*/(${"\"" * 3}<pre><span class="line">${"\"" * 3}),_display_(/*184.54*/(index+1)),format.raw/*184.63*/(${"\"" * 3}</span><span class="code">${"\"" * 3}),_display_(/*184.90*/line),format.raw/*184.94*/(${"\"" * 3}</span></pre>
      |                        ${"\"" * 3})))}}),format.raw/*187.22*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*188.17*/(${"\"" * 3}</div>
      |
      |            ${"\"" * 3})))}/*192.13*/case exception: play.api.PlayException if exception.cause != null =>/*192.81*/ {_display_(Seq[Any](format.raw/*192.83*/(${"\"" * 3}
      |
      |                ${"\"" * 3}),format.raw/*194.17*/(${"\"" * 3}<h2>
      |                    No source available, here is the exception stack trace:
      |                </h2>
      |
      |                <div>
      |
      |                    <pre class="error"><span class="line">-></span><span class="code">${"\"" * 3}),_display_(/*200.88*/exception/*200.97*/.cause.getClass.getName),format.raw/*200.120*/(${"\"" * 3}: ${"\"" * 3}),_display_(/*200.123*/exception/*200.132*/.cause.getMessage),format.raw/*200.149*/(${"\"" * 3}</span></pre>
      |
      |                    ${"\"" * 3}),_display_(/*202.22*/exception/*202.31*/.cause.getStackTrace.map/*202.55*/ { line =>_display_(Seq[Any](format.raw/*202.65*/(${"\"" * 3}
      |                        ${"\"" * 3}),format.raw/*203.25*/(${"\"" * 3}<pre><span class="line">&nbsp;</span><span class="code">    ${"\"" * 3}),_display_(/*203.86*/line),format.raw/*203.90*/(${"\"" * 3}</span></pre>
      |                    ${"\"" * 3})))}),format.raw/*204.22*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*205.17*/(${"\"" * 3}</div>
      |
      |            ${"\"" * 3})))}/*209.13*/case _ =>/*209.22*/ {_display_(Seq[Any](format.raw/*209.24*/(${"\"" * 3}
      |            ${"\"" * 3})))}}),format.raw/*212.10*/(${"\"" * 3}
      |
      |    ${"\"" * 3}),format.raw/*214.5*/(${"\"" * 3}</body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(playEditor:Option[String],error:play.api.UsefulException,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(playEditor,error)(request)
      |
      |  def f:((Option[String],play.api.UsefulException) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (playEditor,error) => (request) => apply(playEditor,error)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/devError.scala.html
      |                  HASH: adb6c0b09b3a79b58dc45a5bb8b9ce6476214882
      |                  MATRIX: 858->146|1059->254|1145->314|1158->319|1184->325|2210->1324|2224->1329|2281->1377|2321->1379|2362->1392|2406->1408|2435->1409|2480->1426|2662->1580|2691->1581|2732->1594|2763->1597|2792->1598|2837->1615|3130->1880|3159->1881|3200->1894|3230->1896|3259->1897|3304->1914|3360->1942|3389->1943|3430->1956|3467->1965|3496->1966|3541->1983|3890->2304|3919->2305|3960->2318|4001->2331|4030->2332|4075->2349|4198->2444|4227->2445|4268->2458|4311->2473|4340->2474|4385->2491|5171->3249|5200->3250|5241->3263|5272->3266|5301->3267|5346->3284|5632->3542|5661->3543|5702->3556|5734->3560|5763->3561|5808->3578|6038->3780|6067->3781|6108->3794|6150->3808|6179->3809|6224->3826|6552->4126|6581->4127|6622->4140|6664->4154|6693->4155|6738->4172|6886->4292|6915->4293|6956->4306|7010->4332|7039->4333|7084->4350|7155->4393|7184->4394|7225->4407|7279->4433|7308->4434|7354->4451|7426->4494|7456->4495|7498->4508|7547->4528|7577->4529|7623->4546|7771->4665|7801->4666|7843->4679|7882->4689|7912->4690|7958->4707|8015->4735|8045->4736|8087->4749|8138->4771|8168->4772|8214->4789|8362->4908|8392->4909|8434->4919|8467->4924|8548->4977|8563->4982|8591->4988|8635->5004|8650->5009|8666->5015|8678->5031|8746->5089|8787->5091|8833->5108|8877->5124|8891->5128|8957->5172|8995->5204|9014->5213|9055->5215|9101->5232|9157->5260|9172->5265|9206->5277|9257->5306|9296->5317|9311->5322|9327->5328|9339->5344|9402->5397|9443->5399|9490->5418|9525->5443|9539->5447|9588->5457|9638->5478|9699->5511|9728->5530|9743->5535|9784->5537|9840->5565|9866->5569|9896->5570|9965->5619|10012->5627|10068->5655|10088->5665|10103->5670|10144->5672|10202->5702|10228->5706|10258->5708|10284->5712|10332->5740|10381->5750|10439->5779|10577->5889|10624->5914|10690->5951|10717->5955|10748->5957|10775->5961|10839->5993|10897->6019|10947->6040|11050->6115|11094->6149|11108->6153|11120->6185|11149->6204|11190->6206|11253->6241|11274->6252|11307->6275|11319->6315|11383->6369|11424->6371|11494->6412|11553->6443|11579->6447|11621->6461|11673->6490|11705->6493|11739->6516|11754->6520|11801->6527|11832->6528|11875->6542|11899->6543|11935->6546|11985->6567|12037->6596|12093->6623|12296->6803|12368->6893|12399->6914|12440->6916|12510->6957|12555->6974|12581->6978|12623->6992|12675->7021|12726->7043|12778->7072|12834->7099|12861->7103|12945->7189|13008->7246|13058->7267|13115->7292|13150->7321|13221->7382|13262->7384|13309->7402|13342->7407|13362->7417|13393->7426|13471->7476|13491->7486|13538->7523|13550->7551|13580->7571|13621->7573|13679->7602|13732->7627|13763->7636|13818->7663|13844->7667|13916->7729|13962->7746|14003->7781|14081->7849|14122->7851|14169->7869|14411->8083|14430->8092|14476->8115|14508->8118|14528->8127|14568->8144|14632->8180|14651->8189|14685->8213|14734->8223|14788->8248|14877->8309|14903->8313|14970->8348|15016->8365|15057->8400|15076->8409|15117->8411|15164->8436|15198->8442
      |                  LINES: 19->5|24->6|27->9|27->9|27->9|29->11|29->11|29->11|29->11|30->12|30->12|30->12|31->13|35->17|35->17|36->18|36->18|36->18|37->19|44->26|44->26|45->27|45->27|45->27|46->28|47->29|47->29|48->30|48->30|48->30|49->31|57->39|57->39|58->40|58->40|58->40|59->41|62->44|62->44|63->45|63->45|63->45|64->46|81->63|81->63|82->64|82->64|82->64|83->65|90->72|90->72|91->73|91->73|91->73|92->74|97->79|97->79|98->80|98->80|98->80|99->81|107->89|107->89|108->90|108->90|108->90|109->91|113->95|113->95|114->96|114->96|114->96|115->97|116->98|116->98|117->99|117->99|117->99|118->100|119->101|119->101|120->102|120->102|120->102|121->103|124->106|124->106|125->107|125->107|125->107|126->108|127->109|127->109|128->110|128->110|128->110|129->111|132->114|132->114|133->115|134->116|136->118|136->118|136->118|138->120|138->120|138->120|138->122|138->122|138->122|139->123|139->123|139->123|139->123|140->126|140->126|140->126|141->127|141->127|141->127|141->127|142->130|144->132|144->132|144->132|144->134|144->134|144->134|146->136|146->136|146->136|146->136|147->137|148->138|148->138|148->138|148->138|149->139|149->139|149->139|150->140|150->140|151->141|151->141|151->141|151->141|152->142|152->142|152->142|152->142|153->143|153->143|154->144|155->145|155->145|155->145|155->145|155->145|155->145|156->146|157->147|158->148|161->151|161->151|161->151|161->153|161->153|161->153|163->155|163->155|163->155|163->157|163->157|163->157|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|164->158|166->162|166->162|166->162|167->163|167->163|167->163|167->163|167->163|167->163|167->163|167->163|167->163|168->166|169->169|170->170|172->172|174->176|174->176|174->176|176->178|176->178|176->178|176->178|179->181|179->181|179->181|179->183|179->183|179->183|180->184|180->184|180->184|180->184|180->184|181->187|182->188|184->192|184->192|184->192|186->194|192->200|192->200|192->200|192->200|192->200|192->200|194->202|194->202|194->202|194->202|195->203|195->203|195->203|196->204|197->205|199->209|199->209|199->209|200->212|202->214
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/devError.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object devError extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[Option[String],play.api.UsefulException,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 500 Internal Server Error responses, in development mode.
      | * This page display the error in the source code context.
      | */
      |  def apply/*5.2*/(playEditor: Option[String], error: play.api.UsefulException)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>${"\"" * 3}),_display_(/*9.17*/error/*9.22*/.title),format.raw/*9.28*/(${"\"" * 3}</title>
      |        <link rel="shortcut icon" href="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAlFJREFUeNqUU8tOFEEUPVVdNV3dPe8xYRBnjGhmBgKjKzCIiQvBoIaNbly5Z+PSv3Aj7DSiP2B0rwkLGVdGgxITSCRIJGSMEQWZR3eVt5sEFBgTb/dN1yvnnHtPNTPG4PqdHgCMXnPRSZrpSuH8vUJu4DE4rYHDGAZDX62BZttHqTiIayM3gGiXQsgYLEvATaqxU+dy1U13YXapXptpNHY8iwn8KyIAzm1KBdtRZWErpI5lEWTXp5Z/vHpZ3/wyKKwYGGOdAYwR0EZwoezTYApBEIObyELl/aE1/83cp40Pt5mxqCKrE4Ck+mVWKKcI5tA8BLEhRBKJLjez6a7MLq7XZtp+yyOawwCBtkiBVZDKzRk4NN7NQBMYPHiZDFhXY+p9ff7F961vVcnl4R5I2ykJ5XFN7Ab7Gc61VoipNBKF+PDyztu5lfrSLT/wIwCxq0CAGtXHZTzqR2jtwQiXONma6hHpj9sLT7YaPxfTXuZdBGA02Wi7FS48YiTfj+i2NhqtdhP5RC8mh2/Op7y0v6eAcWVLFT8D7kWX5S9mepp+C450MV6aWL1cGnvkxbwHtLW2B9AOkLeUd9KEDuh9fl/7CEj7YH5g+3r/lWfF9In7tPz6T4IIwBJOr1SJyIGQMZQbsh5P9uBq5VJtqHh2mo49pdw5WFoEwKWqWHacaWOjQXWGcifKo6vj5RGS6zykI587XeUIQDqJSmAp+lE4qt19W5P9o8+Lma5DcjsC8JiT607lMVkdqQ0Vyh3lHhmh52tfNy78ajXv0rgYzv8nfwswANuk+7sD/Q0aAAAAAElFTkSuQmCC">
      |        ${"\"" * 3}),_display_(/*11.10*/views/*11.15*/.html.helper.style(Symbol("type") -> "text/css")/*11.63*/ {_display_(Seq[Any](format.raw/*11.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*12.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*12.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*12.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*13.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*17.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*18.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*18.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*19.17*/(${"\"" * 3}margin: 0;
      |                background: #A31012;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #690000;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*26.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*27.13*/(${"\"" * 3}a ${"\"" * 3}),format.raw/*27.15*/(${"\"" * 3}{${"\"" * 3}),format.raw/*27.16*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*28.17*/(${"\"" * 3}color: #D36D6D;
      |            ${"\"" * 3}),format.raw/*29.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*29.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*30.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*30.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*30.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*31.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F5A0A0;
      |                border-top: 4px solid #D36D6D;
      |                color: #730000;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7A7A;
      |            ${"\"" * 3}),format.raw/*39.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*39.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*40.13*/(${"\"" * 3}p#detail.pre ${"\"" * 3}),format.raw/*40.26*/(${"\"" * 3}{${"\"" * 3}),format.raw/*40.27*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*41.17*/(${"\"" * 3}white-space: pre;
      |                font-size: 13px;
      |                overflow: auto;
      |            ${"\"" * 3}),format.raw/*44.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*44.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*45.13*/(${"\"" * 3}p#detail input ${"\"" * 3}),format.raw/*45.28*/(${"\"" * 3}{${"\"" * 3}),format.raw/*45.29*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*46.17*/(${"\"" * 3}background: #AE1113;
      |                background: -webkit-linear-gradient(#AE1113, #A31012);
      |                background: -o-linear-gradient(#AE1113, #A31012);
      |                background: -moz-linear-gradient(#AE1113, #A31012);
      |                background: linear-gradient(#AE1113, #A31012);
      |                border: 1px solid #790000;
      |                padding: 3px 10px;
      |                text-shadow: 1px 1px 0 rgba(0, 0, 0, .5);
      |                color: white;
      |                border-radius: 3px;
      |                cursor: pointer;
      |                font-family: Monaco, 'Lucida Console';
      |                font-size: 12px;
      |                margin: 0 10px;
      |                display: inline-block;
      |                position: relative;
      |                top: -1px;
      |            ${"\"" * 3}),format.raw/*63.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*63.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*64.13*/(${"\"" * 3}h2 ${"\"" * 3}),format.raw/*64.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*64.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*65.17*/(${"\"" * 3}margin: 0;
      |                padding: 5px 45px;
      |                font-size: 12px;
      |                background: #333;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-top: 4px solid #2a2a2a;
      |            ${"\"" * 3}),format.raw/*72.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*72.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*73.13*/(${"\"" * 3}pre ${"\"" * 3}),format.raw/*73.17*/(${"\"" * 3}{${"\"" * 3}),format.raw/*73.18*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*74.17*/(${"\"" * 3}margin: 0;
      |                border-bottom: 1px solid #DDD;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
      |                position: relative;
      |                font-size: 12px;
      |            ${"\"" * 3}),format.raw/*79.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*79.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*80.13*/(${"\"" * 3}pre span.line ${"\"" * 3}),format.raw/*80.27*/(${"\"" * 3}{${"\"" * 3}),format.raw/*80.28*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*81.17*/(${"\"" * 3}text-align: right;
      |                display: inline-block;
      |                padding: 5px 5px;
      |                width: 30px;
      |                background: #D6D6D6;
      |                color: #8B8B8B;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
      |                font-weight: bold;
      |            ${"\"" * 3}),format.raw/*89.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*89.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*90.13*/(${"\"" * 3}pre span.code ${"\"" * 3}),format.raw/*90.27*/(${"\"" * 3}{${"\"" * 3}),format.raw/*90.28*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*91.17*/(${"\"" * 3}padding: 5px 5px;
      |                position: absolute;
      |                right: 0;
      |                left: 40px;
      |            ${"\"" * 3}),format.raw/*95.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*95.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*96.13*/(${"\"" * 3}pre:first-child span.code ${"\"" * 3}),format.raw/*96.39*/(${"\"" * 3}{${"\"" * 3}),format.raw/*96.40*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*97.17*/(${"\"" * 3}border-top: 4px solid #CDCDCD;
      |            ${"\"" * 3}),format.raw/*98.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*98.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*99.13*/(${"\"" * 3}pre:first-child span.line ${"\"" * 3}),format.raw/*99.39*/(${"\"" * 3}{${"\"" * 3}),format.raw/*99.40*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*100.17*/(${"\"" * 3}border-top: 4px solid #B6B6B6;
      |            ${"\"" * 3}),format.raw/*101.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*101.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*102.13*/(${"\"" * 3}pre.error span.line ${"\"" * 3}),format.raw/*102.33*/(${"\"" * 3}{${"\"" * 3}),format.raw/*102.34*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*103.17*/(${"\"" * 3}background: #A31012;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |            ${"\"" * 3}),format.raw/*106.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*106.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*107.13*/(${"\"" * 3}pre.error ${"\"" * 3}),format.raw/*107.23*/(${"\"" * 3}{${"\"" * 3}),format.raw/*107.24*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*108.17*/(${"\"" * 3}color: #A31012;
      |            ${"\"" * 3}),format.raw/*109.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*109.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*110.13*/(${"\"" * 3}pre.error span.marker ${"\"" * 3}),format.raw/*110.35*/(${"\"" * 3}{${"\"" * 3}),format.raw/*110.36*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*111.17*/(${"\"" * 3}background: #A31012;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |            ${"\"" * 3}),format.raw/*114.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*114.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*115.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*116.5*/(${"\"" * 3}</head>
      |    <body id="play-error-page">
      |        <h1>${"\"" * 3}),_display_(/*118.14*/error/*118.19*/.title),format.raw/*118.25*/(${"\"" * 3}</h1>
      |
      |        ${"\"" * 3}),_display_(/*120.10*/error/*120.15*/ match/*120.21*/ {/*122.13*/case description:play.api.PlayException.RichDescription =>/*122.71*/ {_display_(Seq[Any](format.raw/*122.73*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*123.17*/(${"\"" * 3}<p id="detail">${"\"" * 3}),_display_(/*123.33*/play/*123.37*/.twirl.api.Html(description.htmlDescription)),format.raw/*123.81*/(${"\"" * 3}</p>
      |            ${"\"" * 3})))}/*126.13*/case _ =>/*126.22*/ {_display_(Seq[Any](format.raw/*126.24*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*127.17*/(${"\"" * 3}<p id="detail" class="pre">${"\"" * 3}),_display_(/*127.45*/error/*127.50*/.description),format.raw/*127.62*/(${"\"" * 3}</p>
      |            ${"\"" * 3})))}}),format.raw/*130.10*/(${"\"" * 3}
      |
      |        ${"\"" * 3}),_display_(/*132.10*/error/*132.15*/ match/*132.21*/ {/*134.13*/case source:play.api.PlayException.ExceptionSource =>/*134.66*/ {_display_(Seq[Any](format.raw/*134.68*/(${"\"" * 3}
      |
      |                ${"\"" * 3}),_display_(/*136.18*/Option(source.sourceName)/*136.43*/.map/*136.47*/ { name =>_display_(Seq[Any](format.raw/*136.57*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*137.21*/(${"\"" * 3}<h2>
      |                        In ${"\"" * 3}),_display_(/*138.29*/Option(source.line)/*138.48*/.fold/*138.53*/ {_display_(Seq[Any](format.raw/*138.55*/(${"\"" * 3}
      |                          ${"\"" * 3}),_display_(/*139.28*/name),format.raw/*139.32*/(${"\"" * 3} ${"\"" * 3}),format.raw/*139.33*/(${"\"" * 3}(line number not found)
      |                        ${"\"" * 3})))}/*140.26*/{line =>_display_(Seq[Any](format.raw/*140.34*/(${"\"" * 3}
      |                          ${"\"" * 3}),_display_(/*141.28*/playEditor/*141.38*/.fold/*141.43*/ {_display_(Seq[Any](format.raw/*141.45*/(${"\"" * 3}
      |                            ${"\"" * 3}),_display_(/*142.30*/name),format.raw/*142.34*/(${"\"" * 3}:${"\"" * 3}),_display_(/*142.36*/line),format.raw/*142.40*/(${"\"" * 3}
      |                          ${"\"" * 3})))}/*143.28*/ { link =>_display_(Seq[Any](format.raw/*143.38*/(${"\"" * 3}
      |                            ${"\"" * 3}),format.raw/*144.29*/(${"\"" * 3}<iframe name="_onlyForFiringEditorLink" style="display:none;"></iframe>
      |                            <a href=${"\"" * 3}"),_display_(/*145.39*/{link.format(name, line)}),format.raw/*145.64*/(${"\"" * 3}" target="_onlyForFiringEditorLink">${"\"" * 3}),_display_(/*145.101*/name),format.raw/*145.105*/(${"\"" * 3}:${"\"" * 3}),_display_(/*145.107*/line),format.raw/*145.111*/(${"\"" * 3}</a>
      |                          ${"\"" * 3})))}),format.raw/*146.28*/(${"\"" * 3}
      |                        ${"\"" * 3})))}),format.raw/*147.26*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*148.21*/(${"\"" * 3}</h2>
      |
      |                    <div id="source-code">
      |                        ${"\"" * 3}),_display_(/*151.26*/Option(source.interestingLines(4))/*151.60*/.map/*151.64*/ {/*153.29*/case interesting =>/*153.48*/ {_display_(Seq[Any](format.raw/*153.50*/(${"\"" * 3}
      |
      |                                ${"\"" * 3}),_display_(/*155.34*/interesting/*155.45*/.focus.zipWithIndex.map/*155.68*/ {/*157.37*/case (line,index) if index == interesting.errorLine =>/*157.91*/ {_display_(Seq[Any](format.raw/*157.93*/(${"\"" * 3}
      |                                        ${"\"" * 3}),format.raw/*158.41*/(${"\"" * 3}<pre class="error" data-file=${"\"" * 3}"),_display_(/*158.72*/name),format.raw/*158.76*/(${"\"" * 3}" data-line=${"\"" * 3}"),_display_(/*158.90*/(interesting.firstLine+index)),format.raw/*158.119*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*158.122*/Option(source.position)/*158.145*/.map/*158.149*/ { c =>_display_(Seq[Any](format.raw/*158.156*/(${"\"" * 3} ${"\"" * 3}),format.raw/*158.157*/(${"\"" * 3}data-column=${"\"" * 3}"),_display_(/*158.171*/c),format.raw/*158.172*/(${"\"" * 3}" ${"\"" * 3})))}),format.raw/*158.175*/(${"\"" * 3}><span class="line">${"\"" * 3}),_display_(/*158.196*/(interesting.firstLine+index)),format.raw/*158.225*/(${"\"" * 3}</span><span class="code">${"\"" * 3}),_display_(/*158.252*/(Option(source.position).map(pos => (line+" ").zipWithIndex.map{ case (c,i) if i == pos => Html(${"\"" * 3}<span class="marker">${"\"" * 3} + c + ${"\"" * 3}</span>${"\"" * 3}); case (c,_) => c}).getOrElse(line))),format.raw/*158.432*/(${"\"" * 3}</span></pre>
      |
      |                                    ${"\"" * 3})))}/*162.37*/case (line, index) =>/*162.58*/ {_display_(Seq[Any](format.raw/*162.60*/(${"\"" * 3}
      |                                        ${"\"" * 3}),format.raw/*163.41*/(${"\"" * 3}<pre data-file=${"\"" * 3}"),_display_(/*163.58*/name),format.raw/*163.62*/(${"\"" * 3}" data-line=${"\"" * 3}"),_display_(/*163.76*/(interesting.firstLine+index)),format.raw/*163.105*/(${"\"" * 3}"><span class="line">${"\"" * 3}),_display_(/*163.127*/(interesting.firstLine+index)),format.raw/*163.156*/(${"\"" * 3}</span><span class="code">${"\"" * 3}),_display_(/*163.183*/line),format.raw/*163.187*/(${"\"" * 3}</span></pre>
      |                                    ${"\"" * 3})))}}),format.raw/*166.34*/(${"\"" * 3}
      |                            ${"\"" * 3})))}}),format.raw/*169.26*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*170.21*/(${"\"" * 3}</div>
      |
      |                ${"\"" * 3})))}),format.raw/*172.18*/(${"\"" * 3}
      |
      |            ${"\"" * 3})))}/*176.13*/case attachment:play.api.PlayException.ExceptionAttachment =>/*176.74*/ {_display_(Seq[Any](format.raw/*176.76*/(${"\"" * 3}
      |
      |                ${"\"" * 3}),format.raw/*178.17*/(${"\"" * 3}<h2>${"\"" * 3}),_display_(/*178.22*/attachment/*178.32*/.subTitle),format.raw/*178.41*/(${"\"" * 3}</h2>
      |
      |                <div>
      |                    ${"\"" * 3}),_display_(/*181.22*/attachment/*181.32*/.content.split("\\n").zipWithIndex.map/*181.69*/ {/*183.25*/case (line,index) =>/*183.45*/ {_display_(Seq[Any](format.raw/*183.47*/(${"\"" * 3}
      |                            ${"\"" * 3}),format.raw/*184.29*/(${"\"" * 3}<pre><span class="line">${"\"" * 3}),_display_(/*184.54*/(index+1)),format.raw/*184.63*/(${"\"" * 3}</span><span class="code">${"\"" * 3}),_display_(/*184.90*/line),format.raw/*184.94*/(${"\"" * 3}</span></pre>
      |                        ${"\"" * 3})))}}),format.raw/*187.22*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*188.17*/(${"\"" * 3}</div>
      |
      |            ${"\"" * 3})))}/*192.13*/case exception: play.api.PlayException if exception.cause != null =>/*192.81*/ {_display_(Seq[Any](format.raw/*192.83*/(${"\"" * 3}
      |
      |                ${"\"" * 3}),format.raw/*194.17*/(${"\"" * 3}<h2>
      |                    No source available, here is the exception stack trace:
      |                </h2>
      |
      |                <div>
      |
      |                    <pre class="error"><span class="line">-></span><span class="code">${"\"" * 3}),_display_(/*200.88*/exception/*200.97*/.cause.getClass.getName),format.raw/*200.120*/(${"\"" * 3}: ${"\"" * 3}),_display_(/*200.123*/exception/*200.132*/.cause.getMessage),format.raw/*200.149*/(${"\"" * 3}</span></pre>
      |
      |                    ${"\"" * 3}),_display_(/*202.22*/exception/*202.31*/.cause.getStackTrace.map/*202.55*/ { line =>_display_(Seq[Any](format.raw/*202.65*/(${"\"" * 3}
      |                        ${"\"" * 3}),format.raw/*203.25*/(${"\"" * 3}<pre><span class="line">&nbsp;</span><span class="code">    ${"\"" * 3}),_display_(/*203.86*/line),format.raw/*203.90*/(${"\"" * 3}</span></pre>
      |                    ${"\"" * 3})))}),format.raw/*204.22*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*205.17*/(${"\"" * 3}</div>
      |
      |            ${"\"" * 3})))}/*209.13*/case _ =>/*209.22*/ {_display_(Seq[Any](format.raw/*209.24*/(${"\"" * 3}
      |            ${"\"" * 3})))}}),format.raw/*212.10*/(${"\"" * 3}
      |
      |    ${"\"" * 3}),format.raw/*214.5*/(${"\"" * 3}</body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(playEditor:Option[String],error:play.api.UsefulException,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(playEditor,error)(request)
      |
      |  def f:((Option[String],play.api.UsefulException) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (playEditor,error) => (request) => apply(playEditor,error)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/devError.scala.html
      |                  HASH: c3f1ff88a7417fee8c441502ff83604d7abb536a
      |                  MATRIX: 820->146|1021->254|1107->314|1120->319|1146->325|2172->1324|2186->1329|2243->1377|2283->1379|2324->1392|2368->1408|2397->1409|2442->1426|2624->1580|2653->1581|2694->1594|2725->1597|2754->1598|2799->1615|3092->1880|3121->1881|3162->1894|3192->1896|3221->1897|3266->1914|3322->1942|3351->1943|3392->1956|3429->1965|3458->1966|3503->1983|3852->2304|3881->2305|3922->2318|3963->2331|3992->2332|4037->2349|4160->2444|4189->2445|4230->2458|4273->2473|4302->2474|4347->2491|5133->3249|5162->3250|5203->3263|5234->3266|5263->3267|5308->3284|5594->3542|5623->3543|5664->3556|5696->3560|5725->3561|5770->3578|6000->3780|6029->3781|6070->3794|6112->3808|6141->3809|6186->3826|6514->4126|6543->4127|6584->4140|6626->4154|6655->4155|6700->4172|6848->4292|6877->4293|6918->4306|6972->4332|7001->4333|7046->4350|7117->4393|7146->4394|7187->4407|7241->4433|7270->4434|7316->4451|7388->4494|7418->4495|7460->4508|7509->4528|7539->4529|7585->4546|7733->4665|7763->4666|7805->4679|7844->4689|7874->4690|7920->4707|7977->4735|8007->4736|8049->4749|8100->4771|8130->4772|8176->4789|8324->4908|8354->4909|8396->4919|8429->4924|8510->4977|8525->4982|8553->4988|8597->5004|8612->5009|8628->5015|8640->5031|8708->5089|8749->5091|8795->5108|8839->5124|8853->5128|8919->5172|8957->5204|8976->5213|9017->5215|9063->5232|9119->5260|9134->5265|9168->5277|9219->5306|9258->5317|9273->5322|9289->5328|9301->5344|9364->5397|9405->5399|9452->5418|9487->5443|9501->5447|9550->5457|9600->5478|9661->5511|9690->5530|9705->5535|9746->5537|9802->5565|9828->5569|9858->5570|9927->5619|9974->5627|10030->5655|10050->5665|10065->5670|10106->5672|10164->5702|10190->5706|10220->5708|10246->5712|10294->5740|10343->5750|10401->5779|10539->5889|10586->5914|10652->5951|10679->5955|10710->5957|10737->5961|10801->5993|10859->6019|10909->6040|11012->6115|11056->6149|11070->6153|11082->6185|11111->6204|11152->6206|11215->6241|11236->6252|11269->6275|11281->6315|11345->6369|11386->6371|11456->6412|11515->6443|11541->6447|11583->6461|11635->6490|11667->6493|11701->6516|11716->6520|11763->6527|11794->6528|11837->6542|11861->6543|11897->6546|11947->6567|11999->6596|12055->6623|12258->6803|12330->6893|12361->6914|12402->6916|12472->6957|12517->6974|12543->6978|12585->6992|12637->7021|12688->7043|12740->7072|12796->7099|12823->7103|12907->7189|12970->7246|13020->7267|13077->7292|13112->7321|13183->7382|13224->7384|13271->7402|13304->7407|13324->7417|13355->7426|13433->7476|13453->7486|13500->7523|13512->7551|13542->7571|13583->7573|13641->7602|13694->7627|13725->7636|13780->7663|13806->7667|13878->7729|13924->7746|13965->7781|14043->7849|14084->7851|14131->7869|14373->8083|14392->8092|14438->8115|14470->8118|14490->8127|14530->8144|14594->8180|14613->8189|14647->8213|14696->8223|14750->8248|14839->8309|14865->8313|14932->8348|14978->8365|15019->8400|15038->8409|15079->8411|15126->8436|15160->8442
      |                  LINES: 18->5|23->6|26->9|26->9|26->9|28->11|28->11|28->11|28->11|29->12|29->12|29->12|30->13|34->17|34->17|35->18|35->18|35->18|36->19|43->26|43->26|44->27|44->27|44->27|45->28|46->29|46->29|47->30|47->30|47->30|48->31|56->39|56->39|57->40|57->40|57->40|58->41|61->44|61->44|62->45|62->45|62->45|63->46|80->63|80->63|81->64|81->64|81->64|82->65|89->72|89->72|90->73|90->73|90->73|91->74|96->79|96->79|97->80|97->80|97->80|98->81|106->89|106->89|107->90|107->90|107->90|108->91|112->95|112->95|113->96|113->96|113->96|114->97|115->98|115->98|116->99|116->99|116->99|117->100|118->101|118->101|119->102|119->102|119->102|120->103|123->106|123->106|124->107|124->107|124->107|125->108|126->109|126->109|127->110|127->110|127->110|128->111|131->114|131->114|132->115|133->116|135->118|135->118|135->118|137->120|137->120|137->120|137->122|137->122|137->122|138->123|138->123|138->123|138->123|139->126|139->126|139->126|140->127|140->127|140->127|140->127|141->130|143->132|143->132|143->132|143->134|143->134|143->134|145->136|145->136|145->136|145->136|146->137|147->138|147->138|147->138|147->138|148->139|148->139|148->139|149->140|149->140|150->141|150->141|150->141|150->141|151->142|151->142|151->142|151->142|152->143|152->143|153->144|154->145|154->145|154->145|154->145|154->145|154->145|155->146|156->147|157->148|160->151|160->151|160->151|160->153|160->153|160->153|162->155|162->155|162->155|162->157|162->157|162->157|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|163->158|165->162|165->162|165->162|166->163|166->163|166->163|166->163|166->163|166->163|166->163|166->163|166->163|167->166|168->169|169->170|171->172|173->176|173->176|173->176|175->178|175->178|175->178|175->178|178->181|178->181|178->181|178->183|178->183|178->183|179->184|179->184|179->184|179->184|179->184|180->187|181->188|183->192|183->192|183->192|185->194|191->200|191->200|191->200|191->200|191->200|191->200|193->202|193->202|193->202|193->202|194->203|194->203|194->203|195->204|196->205|198->209|198->209|198->209|199->212|201->214
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/devNotFound.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object devNotFound extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[String,String,Option[play.api.routing.Router],play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 404 Not Found responses, in development mode.
      | * This page display the routes file content.
      | */
      |  def apply/*5.2*/(method: String, uri: String, router: Option[play.api.routing.Router])(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Action Not Found</title>
      |        <link rel="shortcut icon" href="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAlFJREFUeNqUU8tOFEEUPVVdNV3dPe8xYRBnjGhmBgKjKzCIiQvBoIaNbly5Z+PSv3Aj7DSiP2B0rwkLGVdGgxITSCRIJGSMEQWZR3eVt5sEFBgTb/dN1yvnnHtPNTPG4PqdHgCMXnPRSZrpSuH8vUJu4DE4rYHDGAZDX62BZttHqTiIayM3gGiXQsgYLEvATaqxU+dy1U13YXapXptpNHY8iwn8KyIAzm1KBdtRZWErpI5lEWTXp5Z/vHpZ3/wyKKwYGGOdAYwR0EZwoezTYApBEIObyELl/aE1/83cp40Pt5mxqCKrE4Ck+mVWKKcI5tA8BLEhRBKJLjez6a7MLq7XZtp+yyOawwCBtkiBVZDKzRk4NN7NQBMYPHiZDFhXY+p9ff7F961vVcnl4R5I2ykJ5XFN7Ab7Gc61VoipNBKF+PDyztu5lfrSLT/wIwCxq0CAGtXHZTzqR2jtwQiXONma6hHpj9sLT7YaPxfTXuZdBGA02Wi7FS48YiTfj+i2NhqtdhP5RC8mh2/Op7y0v6eAcWVLFT8D7kWX5S9mepp+C450MV6aWL1cGnvkxbwHtLW2B9AOkLeUd9KEDuh9fl/7CEj7YH5g+3r/lWfF9In7tPz6T4IIwBJOr1SJyIGQMZQbsh5P9uBq5VJtqHh2mo49pdw5WFoEwKWqWHacaWOjQXWGcifKo6vj5RGS6zykI587XeUIQDqJSmAp+lE4qt19W5P9o8+Lma5DcjsC8JiT607lMVkdqQ0Vyh3lHhmh52tfNy78ajXv0rgYzv8nfwswANuk+7sD/Q0aAAAAAElFTkSuQmCC">
      |        ${"\"" * 3}),_display_(/*11.10*/views/*11.15*/.html.helper.style(Symbol("type") -> "text/css")/*11.63*/ {_display_(Seq[Any](format.raw/*11.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*12.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*12.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*12.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*13.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*17.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*18.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*18.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*19.17*/(${"\"" * 3}margin: 0;
      |                background: #AD632A;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #9F5805;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*26.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*27.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*27.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*27.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*28.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F6A960;
      |                border-top: 4px solid #D29052;
      |                color: #733512;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7F5B;
      |            ${"\"" * 3}),format.raw/*36.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*36.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*37.13*/(${"\"" * 3}h2 ${"\"" * 3}),format.raw/*37.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*37.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*38.17*/(${"\"" * 3}margin: 0;
      |                padding: 5px 45px;
      |                font-size: 12px;
      |                background: #333;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-top: 4px solid #2a2a2a;
      |            ${"\"" * 3}),format.raw/*45.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*45.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*46.13*/(${"\"" * 3}pre ${"\"" * 3}),format.raw/*46.17*/(${"\"" * 3}{${"\"" * 3}),format.raw/*46.18*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*47.17*/(${"\"" * 3}margin: 0;
      |                border-bottom: 1px solid #DDD;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
      |                position: relative;
      |                font-size: 12px;
      |            ${"\"" * 3}),format.raw/*52.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*52.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*53.13*/(${"\"" * 3}pre span.line ${"\"" * 3}),format.raw/*53.27*/(${"\"" * 3}{${"\"" * 3}),format.raw/*53.28*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*54.17*/(${"\"" * 3}text-align: right;
      |                display: inline-block;
      |                padding: 5px 5px;
      |                width: 30px;
      |                background: #D6D6D6;
      |                color: #8B8B8B;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
      |                font-weight: bold;
      |            ${"\"" * 3}),format.raw/*62.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*62.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*63.13*/(${"\"" * 3}pre span.route ${"\"" * 3}),format.raw/*63.28*/(${"\"" * 3}{${"\"" * 3}),format.raw/*63.29*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*64.17*/(${"\"" * 3}padding: 5px 5px;
      |                position: absolute;
      |                right: 0;
      |                left: 40px;
      |            ${"\"" * 3}),format.raw/*68.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*68.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*69.13*/(${"\"" * 3}pre span.route span.verb ${"\"" * 3}),format.raw/*69.38*/(${"\"" * 3}{${"\"" * 3}),format.raw/*69.39*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*70.17*/(${"\"" * 3}display: inline-block;
      |                width: 5%;
      |                min-width: 50px;
      |                overflow: hidden;
      |                margin-right: 10px;
      |            ${"\"" * 3}),format.raw/*75.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*75.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*76.13*/(${"\"" * 3}pre span.route span.path ${"\"" * 3}),format.raw/*76.38*/(${"\"" * 3}{${"\"" * 3}),format.raw/*76.39*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*77.17*/(${"\"" * 3}display: inline-block;
      |                width: 30%;
      |                min-width: 200px;
      |                overflow: hidden;
      |                margin-right: 10px;
      |            ${"\"" * 3}),format.raw/*82.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*82.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*83.13*/(${"\"" * 3}pre span.route span.call ${"\"" * 3}),format.raw/*83.38*/(${"\"" * 3}{${"\"" * 3}),format.raw/*83.39*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*84.17*/(${"\"" * 3}display: inline-block;
      |                width: 50%;
      |                overflow: hidden;
      |                margin-right: 10px;
      |            ${"\"" * 3}),format.raw/*88.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*88.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*89.13*/(${"\"" * 3}pre:first-child span.route ${"\"" * 3}),format.raw/*89.40*/(${"\"" * 3}{${"\"" * 3}),format.raw/*89.41*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*90.17*/(${"\"" * 3}border-top: 4px solid #CDCDCD;
      |            ${"\"" * 3}),format.raw/*91.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*91.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*92.13*/(${"\"" * 3}pre:first-child span.line ${"\"" * 3}),format.raw/*92.39*/(${"\"" * 3}{${"\"" * 3}),format.raw/*92.40*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*93.17*/(${"\"" * 3}border-top: 4px solid #B6B6B6;
      |            ${"\"" * 3}),format.raw/*94.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*94.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*95.13*/(${"\"" * 3}pre.error span.line ${"\"" * 3}),format.raw/*95.33*/(${"\"" * 3}{${"\"" * 3}),format.raw/*95.34*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*96.17*/(${"\"" * 3}background: #A31012;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |            ${"\"" * 3}),format.raw/*99.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*99.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*100.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*101.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Action Not Found</h1>
      |
      |        <p id="detail">
      |            For request '${"\"" * 3}),_display_(/*106.27*/method),format.raw/*106.33*/(${"\"" * 3} ${"\"" * 3}),_display_(/*106.35*/uri),format.raw/*106.38*/(${"\"" * 3}'
      |        </p>
      |
      |        ${"\"" * 3}),_display_(/*109.10*/router/*109.16*/ match/*109.22*/ {/*111.13*/case Some(routes) =>/*111.33*/ {_display_(Seq[Any](format.raw/*111.35*/(${"\"" * 3}
      |
      |                ${"\"" * 3}),format.raw/*113.17*/(${"\"" * 3}<h2>
      |                    These routes have been tried, in this order:
      |                </h2>
      |
      |                <div>
      |                    ${"\"" * 3}),_display_(/*118.22*/routes/*118.28*/.documentation.zipWithIndex.map/*118.59*/ { r =>_display_(Seq[Any](format.raw/*118.66*/(${"\"" * 3}
      |                        ${"\"" * 3}),format.raw/*119.25*/(${"\"" * 3}<pre><span class="line">${"\"" * 3}),_display_(/*119.50*/(r._2 + 1)),format.raw/*119.60*/(${"\"" * 3}</span><span class="route"><span class="verb">${"\"" * 3}),_display_(/*119.107*/r/*119.108*/._1._1),format.raw/*119.114*/(${"\"" * 3}</span><span class="path">${"\"" * 3}),_display_(/*119.141*/r/*119.142*/._1._2),format.raw/*119.148*/(${"\"" * 3}</span><span class="call">${"\"" * 3}),_display_(/*119.175*/r/*119.176*/._1._3),format.raw/*119.182*/(${"\"" * 3}</span></span></pre>
      |                    ${"\"" * 3})))}),format.raw/*120.22*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*121.17*/(${"\"" * 3}</div>
      |
      |            ${"\"" * 3})))}/*125.13*/case None =>/*125.25*/ {_display_(Seq[Any](format.raw/*125.27*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*126.17*/(${"\"" * 3}<h2>
      |                    No router defined.
      |                </h2>
      |            ${"\"" * 3})))}}),format.raw/*131.10*/(${"\"" * 3}
      |
      |    ${"\"" * 3}),format.raw/*133.5*/(${"\"" * 3}</body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(method:String,uri:String,router:Option[play.api.routing.Router],request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(method,uri,router)(request)
      |
      |  def f:((String,String,Option[play.api.routing.Router]) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (method,uri,router) => (request) => apply(method,uri,router)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/devNotFound.scala.html
      |                  HASH: 30701b83c9e383f3c65cf06911ce4ded9518e807
      |                  MATRIX: 842->121|1052->238|2153->1312|2167->1317|2224->1365|2264->1367|2305->1380|2349->1396|2378->1397|2423->1414|2605->1568|2634->1569|2675->1582|2706->1585|2735->1586|2780->1603|3073->1868|3102->1869|3143->1882|3180->1891|3209->1892|3254->1909|3603->2230|3632->2231|3673->2244|3704->2247|3733->2248|3778->2265|4064->2523|4093->2524|4134->2537|4166->2541|4195->2542|4240->2559|4470->2761|4499->2762|4540->2775|4582->2789|4611->2790|4656->2807|4984->3107|5013->3108|5054->3121|5097->3136|5126->3137|5171->3154|5319->3274|5348->3275|5389->3288|5442->3313|5471->3314|5516->3331|5709->3496|5738->3497|5779->3510|5832->3535|5861->3536|5906->3553|6101->3720|6130->3721|6171->3734|6224->3759|6253->3760|6298->3777|6459->3910|6488->3911|6529->3924|6584->3951|6613->3952|6658->3969|6729->4012|6758->4013|6799->4026|6853->4052|6882->4053|6927->4070|6998->4113|7027->4114|7068->4127|7116->4147|7145->4148|7190->4165|7337->4284|7366->4285|7408->4295|7441->4300|7573->4404|7601->4410|7631->4412|7656->4415|7709->4440|7725->4446|7741->4452|7753->4468|7783->4488|7824->4490|7871->4508|8035->4644|8051->4650|8092->4681|8138->4688|8192->4713|8245->4738|8277->4748|8353->4795|8365->4796|8394->4802|8450->4829|8462->4830|8491->4836|8547->4863|8559->4864|8588->4870|8662->4912|8708->4929|8749->4964|8771->4976|8812->4978|8858->4995|8970->5085|9004->5091
      |                  LINES: 19->5|24->6|29->11|29->11|29->11|29->11|30->12|30->12|30->12|31->13|35->17|35->17|36->18|36->18|36->18|37->19|44->26|44->26|45->27|45->27|45->27|46->28|54->36|54->36|55->37|55->37|55->37|56->38|63->45|63->45|64->46|64->46|64->46|65->47|70->52|70->52|71->53|71->53|71->53|72->54|80->62|80->62|81->63|81->63|81->63|82->64|86->68|86->68|87->69|87->69|87->69|88->70|93->75|93->75|94->76|94->76|94->76|95->77|100->82|100->82|101->83|101->83|101->83|102->84|106->88|106->88|107->89|107->89|107->89|108->90|109->91|109->91|110->92|110->92|110->92|111->93|112->94|112->94|113->95|113->95|113->95|114->96|117->99|117->99|118->100|119->101|124->106|124->106|124->106|124->106|127->109|127->109|127->109|127->111|127->111|127->111|129->113|134->118|134->118|134->118|134->118|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|135->119|136->120|137->121|139->125|139->125|139->125|140->126|143->131|145->133
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/devNotFound.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object devNotFound extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[String,String,Option[play.api.routing.Router],play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 404 Not Found responses, in development mode.
      | * This page display the routes file content.
      | */
      |  def apply/*5.2*/(method: String, uri: String, router: Option[play.api.routing.Router])(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Action Not Found</title>
      |        <link rel="shortcut icon" href="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAlFJREFUeNqUU8tOFEEUPVVdNV3dPe8xYRBnjGhmBgKjKzCIiQvBoIaNbly5Z+PSv3Aj7DSiP2B0rwkLGVdGgxITSCRIJGSMEQWZR3eVt5sEFBgTb/dN1yvnnHtPNTPG4PqdHgCMXnPRSZrpSuH8vUJu4DE4rYHDGAZDX62BZttHqTiIayM3gGiXQsgYLEvATaqxU+dy1U13YXapXptpNHY8iwn8KyIAzm1KBdtRZWErpI5lEWTXp5Z/vHpZ3/wyKKwYGGOdAYwR0EZwoezTYApBEIObyELl/aE1/83cp40Pt5mxqCKrE4Ck+mVWKKcI5tA8BLEhRBKJLjez6a7MLq7XZtp+yyOawwCBtkiBVZDKzRk4NN7NQBMYPHiZDFhXY+p9ff7F961vVcnl4R5I2ykJ5XFN7Ab7Gc61VoipNBKF+PDyztu5lfrSLT/wIwCxq0CAGtXHZTzqR2jtwQiXONma6hHpj9sLT7YaPxfTXuZdBGA02Wi7FS48YiTfj+i2NhqtdhP5RC8mh2/Op7y0v6eAcWVLFT8D7kWX5S9mepp+C450MV6aWL1cGnvkxbwHtLW2B9AOkLeUd9KEDuh9fl/7CEj7YH5g+3r/lWfF9In7tPz6T4IIwBJOr1SJyIGQMZQbsh5P9uBq5VJtqHh2mo49pdw5WFoEwKWqWHacaWOjQXWGcifKo6vj5RGS6zykI587XeUIQDqJSmAp+lE4qt19W5P9o8+Lma5DcjsC8JiT607lMVkdqQ0Vyh3lHhmh52tfNy78ajXv0rgYzv8nfwswANuk+7sD/Q0aAAAAAElFTkSuQmCC">
      |        ${"\"" * 3}),_display_(/*11.10*/views/*11.15*/.html.helper.style(Symbol("type") -> "text/css")/*11.63*/ {_display_(Seq[Any](format.raw/*11.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*12.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*12.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*12.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*13.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*17.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*18.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*18.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*19.17*/(${"\"" * 3}margin: 0;
      |                background: #AD632A;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #9F5805;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*26.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*27.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*27.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*27.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*28.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F6A960;
      |                border-top: 4px solid #D29052;
      |                color: #733512;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7F5B;
      |            ${"\"" * 3}),format.raw/*36.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*36.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*37.13*/(${"\"" * 3}h2 ${"\"" * 3}),format.raw/*37.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*37.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*38.17*/(${"\"" * 3}margin: 0;
      |                padding: 5px 45px;
      |                font-size: 12px;
      |                background: #333;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-top: 4px solid #2a2a2a;
      |            ${"\"" * 3}),format.raw/*45.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*45.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*46.13*/(${"\"" * 3}pre ${"\"" * 3}),format.raw/*46.17*/(${"\"" * 3}{${"\"" * 3}),format.raw/*46.18*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*47.17*/(${"\"" * 3}margin: 0;
      |                border-bottom: 1px solid #DDD;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
      |                position: relative;
      |                font-size: 12px;
      |            ${"\"" * 3}),format.raw/*52.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*52.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*53.13*/(${"\"" * 3}pre span.line ${"\"" * 3}),format.raw/*53.27*/(${"\"" * 3}{${"\"" * 3}),format.raw/*53.28*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*54.17*/(${"\"" * 3}text-align: right;
      |                display: inline-block;
      |                padding: 5px 5px;
      |                width: 30px;
      |                background: #D6D6D6;
      |                color: #8B8B8B;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.5);
      |                font-weight: bold;
      |            ${"\"" * 3}),format.raw/*62.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*62.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*63.13*/(${"\"" * 3}pre span.route ${"\"" * 3}),format.raw/*63.28*/(${"\"" * 3}{${"\"" * 3}),format.raw/*63.29*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*64.17*/(${"\"" * 3}padding: 5px 5px;
      |                position: absolute;
      |                right: 0;
      |                left: 40px;
      |            ${"\"" * 3}),format.raw/*68.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*68.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*69.13*/(${"\"" * 3}pre span.route span.verb ${"\"" * 3}),format.raw/*69.38*/(${"\"" * 3}{${"\"" * 3}),format.raw/*69.39*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*70.17*/(${"\"" * 3}display: inline-block;
      |                width: 5%;
      |                min-width: 50px;
      |                overflow: hidden;
      |                margin-right: 10px;
      |            ${"\"" * 3}),format.raw/*75.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*75.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*76.13*/(${"\"" * 3}pre span.route span.path ${"\"" * 3}),format.raw/*76.38*/(${"\"" * 3}{${"\"" * 3}),format.raw/*76.39*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*77.17*/(${"\"" * 3}display: inline-block;
      |                width: 30%;
      |                min-width: 200px;
      |                overflow: hidden;
      |                margin-right: 10px;
      |            ${"\"" * 3}),format.raw/*82.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*82.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*83.13*/(${"\"" * 3}pre span.route span.call ${"\"" * 3}),format.raw/*83.38*/(${"\"" * 3}{${"\"" * 3}),format.raw/*83.39*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*84.17*/(${"\"" * 3}display: inline-block;
      |                width: 50%;
      |                overflow: hidden;
      |                margin-right: 10px;
      |            ${"\"" * 3}),format.raw/*88.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*88.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*89.13*/(${"\"" * 3}pre:first-child span.route ${"\"" * 3}),format.raw/*89.40*/(${"\"" * 3}{${"\"" * 3}),format.raw/*89.41*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*90.17*/(${"\"" * 3}border-top: 4px solid #CDCDCD;
      |            ${"\"" * 3}),format.raw/*91.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*91.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*92.13*/(${"\"" * 3}pre:first-child span.line ${"\"" * 3}),format.raw/*92.39*/(${"\"" * 3}{${"\"" * 3}),format.raw/*92.40*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*93.17*/(${"\"" * 3}border-top: 4px solid #B6B6B6;
      |            ${"\"" * 3}),format.raw/*94.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*94.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*95.13*/(${"\"" * 3}pre.error span.line ${"\"" * 3}),format.raw/*95.33*/(${"\"" * 3}{${"\"" * 3}),format.raw/*95.34*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*96.17*/(${"\"" * 3}background: #A31012;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |            ${"\"" * 3}),format.raw/*99.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*99.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*100.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*101.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Action Not Found</h1>
      |
      |        <p id="detail">
      |            For request '${"\"" * 3}),_display_(/*106.27*/method),format.raw/*106.33*/(${"\"" * 3} ${"\"" * 3}),_display_(/*106.35*/uri),format.raw/*106.38*/(${"\"" * 3}'
      |        </p>
      |
      |        ${"\"" * 3}),_display_(/*109.10*/router/*109.16*/ match/*109.22*/ {/*111.13*/case Some(routes) =>/*111.33*/ {_display_(Seq[Any](format.raw/*111.35*/(${"\"" * 3}
      |
      |                ${"\"" * 3}),format.raw/*113.17*/(${"\"" * 3}<h2>
      |                    These routes have been tried, in this order:
      |                </h2>
      |
      |                <div>
      |                    ${"\"" * 3}),_display_(/*118.22*/routes/*118.28*/.documentation.zipWithIndex.map/*118.59*/ { r =>_display_(Seq[Any](format.raw/*118.66*/(${"\"" * 3}
      |                        ${"\"" * 3}),format.raw/*119.25*/(${"\"" * 3}<pre><span class="line">${"\"" * 3}),_display_(/*119.50*/(r._2 + 1)),format.raw/*119.60*/(${"\"" * 3}</span><span class="route"><span class="verb">${"\"" * 3}),_display_(/*119.107*/r/*119.108*/._1._1),format.raw/*119.114*/(${"\"" * 3}</span><span class="path">${"\"" * 3}),_display_(/*119.141*/r/*119.142*/._1._2),format.raw/*119.148*/(${"\"" * 3}</span><span class="call">${"\"" * 3}),_display_(/*119.175*/r/*119.176*/._1._3),format.raw/*119.182*/(${"\"" * 3}</span></span></pre>
      |                    ${"\"" * 3})))}),format.raw/*120.22*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*121.17*/(${"\"" * 3}</div>
      |
      |            ${"\"" * 3})))}/*125.13*/case None =>/*125.25*/ {_display_(Seq[Any](format.raw/*125.27*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*126.17*/(${"\"" * 3}<h2>
      |                    No router defined.
      |                </h2>
      |            ${"\"" * 3})))}}),format.raw/*131.10*/(${"\"" * 3}
      |
      |    ${"\"" * 3}),format.raw/*133.5*/(${"\"" * 3}</body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(method:String,uri:String,router:Option[play.api.routing.Router],request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(method,uri,router)(request)
      |
      |  def f:((String,String,Option[play.api.routing.Router]) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (method,uri,router) => (request) => apply(method,uri,router)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/devNotFound.scala.html
      |                  HASH: d30064cab3d05ba9bcd3c85b52f54d325541bbfd
      |                  MATRIX: 804->121|1014->238|2115->1312|2129->1317|2186->1365|2226->1367|2267->1380|2311->1396|2340->1397|2385->1414|2567->1568|2596->1569|2637->1582|2668->1585|2697->1586|2742->1603|3035->1868|3064->1869|3105->1882|3142->1891|3171->1892|3216->1909|3565->2230|3594->2231|3635->2244|3666->2247|3695->2248|3740->2265|4026->2523|4055->2524|4096->2537|4128->2541|4157->2542|4202->2559|4432->2761|4461->2762|4502->2775|4544->2789|4573->2790|4618->2807|4946->3107|4975->3108|5016->3121|5059->3136|5088->3137|5133->3154|5281->3274|5310->3275|5351->3288|5404->3313|5433->3314|5478->3331|5671->3496|5700->3497|5741->3510|5794->3535|5823->3536|5868->3553|6063->3720|6092->3721|6133->3734|6186->3759|6215->3760|6260->3777|6421->3910|6450->3911|6491->3924|6546->3951|6575->3952|6620->3969|6691->4012|6720->4013|6761->4026|6815->4052|6844->4053|6889->4070|6960->4113|6989->4114|7030->4127|7078->4147|7107->4148|7152->4165|7299->4284|7328->4285|7370->4295|7403->4300|7535->4404|7563->4410|7593->4412|7618->4415|7671->4440|7687->4446|7703->4452|7715->4468|7745->4488|7786->4490|7833->4508|7997->4644|8013->4650|8054->4681|8100->4688|8154->4713|8207->4738|8239->4748|8315->4795|8327->4796|8356->4802|8412->4829|8424->4830|8453->4836|8509->4863|8521->4864|8550->4870|8624->4912|8670->4929|8711->4964|8733->4976|8774->4978|8820->4995|8932->5085|8966->5091
      |                  LINES: 18->5|23->6|28->11|28->11|28->11|28->11|29->12|29->12|29->12|30->13|34->17|34->17|35->18|35->18|35->18|36->19|43->26|43->26|44->27|44->27|44->27|45->28|53->36|53->36|54->37|54->37|54->37|55->38|62->45|62->45|63->46|63->46|63->46|64->47|69->52|69->52|70->53|70->53|70->53|71->54|79->62|79->62|80->63|80->63|80->63|81->64|85->68|85->68|86->69|86->69|86->69|87->70|92->75|92->75|93->76|93->76|93->76|94->77|99->82|99->82|100->83|100->83|100->83|101->84|105->88|105->88|106->89|106->89|106->89|107->90|108->91|108->91|109->92|109->92|109->92|110->93|111->94|111->94|112->95|112->95|112->95|113->96|116->99|116->99|117->100|118->101|123->106|123->106|123->106|123->106|126->109|126->109|126->109|126->111|126->111|126->111|128->113|133->118|133->118|133->118|133->118|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|134->119|135->120|136->121|138->125|138->125|138->125|139->126|142->131|144->133
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/error.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object error extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template2[play.api.UsefulException,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 500 Internal Server Error responses, in production mode.
      | */
      |  def apply/*4.2*/(error: play.api.UsefulException)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Error</title>
      |        ${"\"" * 3}),_display_(/*10.10*/views/*10.15*/.html.helper.style(Symbol("type") -> "text/css")/*10.63*/ {_display_(Seq[Any](format.raw/*10.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*11.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*11.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*11.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*12.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*16.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*17.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}margin: 0;
      |                background: #A31012;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #690000;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*25.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*26.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*26.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*27.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F5A0A0;
      |                border-top: 4px solid #D36D6D;
      |                color: #730000;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7A7A;
      |            ${"\"" * 3}),format.raw/*35.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*35.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*36.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*37.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Oops, an error occurred</h1>
      |
      |        <p id="detail">
      |            This exception has been logged with id <strong>${"\"" * 3}),_display_(/*42.61*/error/*42.66*/.id),format.raw/*42.69*/(${"\"" * 3}</strong>.
      |        </p>
      |
      |    </body>
      |</html>${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(error:play.api.UsefulException,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(error)(request)
      |
      |  def f:((play.api.UsefulException) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (error) => (request) => apply(error)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/error.scala.html
      |                  HASH: 335f72d5040a9baa98752d9038b2520d9579f4ba
      |                  MATRIX: 780->86|953->166|980->167|1089->249|1103->254|1160->302|1200->304|1241->317|1285->333|1314->334|1359->351|1541->505|1570->506|1611->519|1642->522|1671->523|1716->540|2009->805|2038->806|2079->819|2116->828|2145->829|2190->846|2539->1167|2568->1168|2609->1178|2641->1183|2813->1328|2827->1333|2851->1336
      |                  LINES: 18->4|23->5|24->6|28->10|28->10|28->10|28->10|29->11|29->11|29->11|30->12|34->16|34->16|35->17|35->17|35->17|36->18|43->25|43->25|44->26|44->26|44->26|45->27|53->35|53->35|54->36|55->37|60->42|60->42|60->42
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/error.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object error extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template2[play.api.UsefulException,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 500 Internal Server Error responses, in production mode.
      | */
      |  def apply/*4.2*/(error: play.api.UsefulException)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Error</title>
      |        ${"\"" * 3}),_display_(/*10.10*/views/*10.15*/.html.helper.style(Symbol("type") -> "text/css")/*10.63*/ {_display_(Seq[Any](format.raw/*10.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*11.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*11.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*11.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*12.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*16.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*17.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}margin: 0;
      |                background: #A31012;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #690000;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*25.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*26.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*26.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*27.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F5A0A0;
      |                border-top: 4px solid #D36D6D;
      |                color: #730000;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7A7A;
      |            ${"\"" * 3}),format.raw/*35.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*35.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*36.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*37.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Oops, an error occurred</h1>
      |
      |        <p id="detail">
      |            This exception has been logged with id <strong>${"\"" * 3}),_display_(/*42.61*/error/*42.66*/.id),format.raw/*42.69*/(${"\"" * 3}</strong>.
      |        </p>
      |
      |    </body>
      |</html>${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(error:play.api.UsefulException,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(error)(request)
      |
      |  def f:((play.api.UsefulException) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (error) => (request) => apply(error)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/error.scala.html
      |                  HASH: bc260cdda4d635148bce2722e0d259beefde17e5
      |                  MATRIX: 742->86|915->166|942->167|1051->249|1065->254|1122->302|1162->304|1203->317|1247->333|1276->334|1321->351|1503->505|1532->506|1573->519|1604->522|1633->523|1678->540|1971->805|2000->806|2041->819|2078->828|2107->829|2152->846|2501->1167|2530->1168|2571->1178|2603->1183|2775->1328|2789->1333|2813->1336
      |                  LINES: 17->4|22->5|23->6|27->10|27->10|27->10|27->10|28->11|28->11|28->11|29->12|33->16|33->16|34->17|34->17|34->17|35->18|42->25|42->25|43->26|43->26|43->26|44->27|52->35|52->35|53->36|54->37|59->42|59->42|59->42
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/notFound.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object notFound extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[String,String,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 404 Not Found responses, in production mode.
      | */
      |  def apply/*4.2*/(method: String, uri: String)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Not Found</title>
      |        ${"\"" * 3}),_display_(/*9.10*/views/*9.15*/.html.helper.style(Symbol("type") -> "text/css")/*9.63*/ {_display_(Seq[Any](format.raw/*9.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*10.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*10.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*10.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*11.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*15.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*15.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*16.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*16.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}margin: 0;
      |                background: #AD632A;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #9F5805;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*24.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*24.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*25.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*25.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*26.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F6A960;
      |                border-top: 4px solid #D29052;
      |                color: #733512;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7F5B;
      |            ${"\"" * 3}),format.raw/*34.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*34.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*35.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*36.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Not Found</h1>
      |
      |        <p id="detail">
      |            For request '${"\"" * 3}),_display_(/*41.27*/method),format.raw/*41.33*/(${"\"" * 3} ${"\"" * 3}),_display_(/*41.35*/uri),format.raw/*41.38*/(${"\"" * 3}'
      |        </p>
      |
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(method:String,uri:String,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(method,uri)(request)
      |
      |  def f:((String,String) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (method,uri) => (request) => apply(method,uri)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/notFound.scala.html
      |                  HASH: da96cb6177a01db61c2cf5b0518972456dc83765
      |                  MATRIX: 760->74|929->150|1041->236|1054->241|1110->289|1149->291|1190->304|1234->320|1263->321|1308->338|1490->492|1519->493|1560->506|1591->509|1620->510|1665->527|1958->792|1987->793|2028->806|2065->815|2094->816|2139->833|2488->1154|2517->1155|2558->1165|2590->1170|2714->1267|2741->1273|2770->1275|2794->1278
      |                  LINES: 18->4|23->5|27->9|27->9|27->9|27->9|28->10|28->10|28->10|29->11|33->15|33->15|34->16|34->16|34->16|35->17|42->24|42->24|43->25|43->25|43->25|44->26|52->34|52->34|53->35|54->36|59->41|59->41|59->41|59->41
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/notFound.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object notFound extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[String,String,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 404 Not Found responses, in production mode.
      | */
      |  def apply/*4.2*/(method: String, uri: String)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Not Found</title>
      |        ${"\"" * 3}),_display_(/*9.10*/views/*9.15*/.html.helper.style(Symbol("type") -> "text/css")/*9.63*/ {_display_(Seq[Any](format.raw/*9.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*10.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*10.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*10.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*11.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*15.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*15.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*16.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*16.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}margin: 0;
      |                background: #AD632A;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #9F5805;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*24.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*24.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*25.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*25.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*26.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #F6A960;
      |                border-top: 4px solid #D29052;
      |                color: #733512;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #BA7F5B;
      |            ${"\"" * 3}),format.raw/*34.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*34.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*35.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*36.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Not Found</h1>
      |
      |        <p id="detail">
      |            For request '${"\"" * 3}),_display_(/*41.27*/method),format.raw/*41.33*/(${"\"" * 3} ${"\"" * 3}),_display_(/*41.35*/uri),format.raw/*41.38*/(${"\"" * 3}'
      |        </p>
      |
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(method:String,uri:String,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(method,uri)(request)
      |
      |  def f:((String,String) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (method,uri) => (request) => apply(method,uri)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/notFound.scala.html
      |                  HASH: 03a1cf7f522fd09df0170189ee6dde0f40dd10e5
      |                  MATRIX: 722->74|891->150|1003->236|1016->241|1072->289|1111->291|1152->304|1196->320|1225->321|1270->338|1452->492|1481->493|1522->506|1553->509|1582->510|1627->527|1920->792|1949->793|1990->806|2027->815|2056->816|2101->833|2450->1154|2479->1155|2520->1165|2552->1170|2676->1267|2703->1273|2732->1275|2756->1278
      |                  LINES: 17->4|22->5|26->9|26->9|26->9|26->9|27->10|27->10|27->10|28->11|32->15|32->15|33->16|33->16|33->16|34->17|41->24|41->24|42->25|42->25|42->25|43->26|51->34|51->34|52->35|53->36|58->41|58->41|58->41|58->41
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/todo.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object todo extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 501 Not Implemented responses.
      | */
      |  def apply/*4.2*/()(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html>
      |    <head>
      |        <title>TODO</title>
      |        ${"\"" * 3}),_display_(/*10.10*/views/*10.15*/.html.helper.style(Symbol("type") -> "text/css")/*10.63*/ {_display_(Seq[Any](format.raw/*10.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*11.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*11.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*11.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*12.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*16.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*17.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}margin: 0;
      |                background: #533CAD;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #3A0B9F;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*25.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*26.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*26.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*27.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #BCACF6;
      |                border-top: 4px solid #7365B6;
      |                color: #312073;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #39325B;
      |            ${"\"" * 3}),format.raw/*35.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*35.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*36.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*37.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>TODO</h1>
      |
      |        <p id="detail">
      |            Action not implemented yet.
      |        </p>
      |
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply()(request)
      |
      |  def f:(() => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = () => (request) => apply()(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/todo.scala.html
      |                  HASH: 4ee9bf8161692a50dcec6780a81f51a957a1ff3e
      |                  MATRIX: 728->60|870->109|897->110|995->181|1009->186|1066->234|1106->236|1147->249|1191->265|1220->266|1265->283|1447->437|1476->438|1517->451|1548->454|1577->455|1622->472|1915->737|1944->738|1985->751|2022->760|2051->761|2096->778|2445->1099|2474->1100|2515->1110|2547->1115
      |                  LINES: 18->4|23->5|24->6|28->10|28->10|28->10|28->10|29->11|29->11|29->11|30->12|34->16|34->16|35->17|35->17|35->17|36->18|43->25|43->25|44->26|44->26|44->26|45->27|53->35|53->35|54->36|55->37
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/todo.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object todo extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 501 Not Implemented responses.
      | */
      |  def apply/*4.2*/()(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html>
      |    <head>
      |        <title>TODO</title>
      |        ${"\"" * 3}),_display_(/*10.10*/views/*10.15*/.html.helper.style(Symbol("type") -> "text/css")/*10.63*/ {_display_(Seq[Any](format.raw/*10.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*11.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*11.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*11.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*12.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*16.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*17.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}margin: 0;
      |                background: #533CAD;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #3A0B9F;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*25.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*26.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*26.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*27.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #BCACF6;
      |                border-top: 4px solid #7365B6;
      |                color: #312073;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #39325B;
      |            ${"\"" * 3}),format.raw/*35.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*35.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*36.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*37.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>TODO</h1>
      |
      |        <p id="detail">
      |            Action not implemented yet.
      |        </p>
      |
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply()(request)
      |
      |  def f:(() => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = () => (request) => apply()(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/todo.scala.html
      |                  HASH: e6e3a6b48b6285dd950ff9c5b09d0d897e161d9e
      |                  MATRIX: 690->60|832->109|859->110|957->181|971->186|1028->234|1068->236|1109->249|1153->265|1182->266|1227->283|1409->437|1438->438|1479->451|1510->454|1539->455|1584->472|1877->737|1906->738|1947->751|1984->760|2013->761|2058->778|2407->1099|2436->1100|2477->1110|2509->1115
      |                  LINES: 17->4|22->5|23->6|27->10|27->10|27->10|27->10|28->11|28->11|28->11|29->12|33->16|33->16|34->17|34->17|34->17|35->18|42->25|42->25|43->26|43->26|43->26|44->27|52->35|52->35|53->36|54->37
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/unauthorized.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object unauthorized extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 401 Not Authorized responses.
      | */
      |  def apply/*4.2*/()(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Unauthorized</title>
      |        ${"\"" * 3}),_display_(/*10.10*/views/*10.15*/.html.helper.style(Symbol("type") -> "text/css")/*10.63*/ {_display_(Seq[Any](format.raw/*10.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*11.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*11.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*11.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*12.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*16.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*17.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}margin: 0;
      |                background: #333;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #111;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*25.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*26.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*26.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*27.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #888;
      |                border-top: 4px solid #666;
      |                color: #111;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #333;
      |            ${"\"" * 3}),format.raw/*35.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*35.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*36.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*37.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Unauthorized</h1>
      |        <p id="detail">
      |            You must be authenticated to access this page.
      |        </p>
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply()(request)
      |
      |  def f:(() => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = () => (request) => apply()(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/unauthorized.scala.html
      |                  HASH: 1087688b06a9cb9b38d6fe09f34e6e7b74075283
      |                  MATRIX: 735->59|877->108|904->109|1020->198|1034->203|1091->251|1131->253|1172->266|1216->282|1245->283|1290->300|1472->454|1501->455|1542->468|1573->471|1602->472|1647->489|1934->748|1963->749|2004->762|2041->771|2070->772|2115->789|2452->1098|2481->1099|2522->1109|2554->1114
      |                  LINES: 18->4|23->5|24->6|28->10|28->10|28->10|28->10|29->11|29->11|29->11|30->12|34->16|34->16|35->17|35->17|35->17|36->18|43->25|43->25|44->26|44->26|44->26|45->27|53->35|53->35|54->36|55->37
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/defaultpages/unauthorized.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.defaultpages
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object unauthorized extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default page for 401 Not Authorized responses.
      | */
      |  def apply/*4.2*/()(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*5.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*6.1*/(${"\"" * 3}<!DOCTYPE html>
      |<html lang="en">
      |    <head>
      |        <title>Unauthorized</title>
      |        ${"\"" * 3}),_display_(/*10.10*/views/*10.15*/.html.helper.style(Symbol("type") -> "text/css")/*10.63*/ {_display_(Seq[Any](format.raw/*10.65*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*11.13*/(${"\"" * 3}html, body, pre ${"\"" * 3}),format.raw/*11.29*/(${"\"" * 3}{${"\"" * 3}),format.raw/*11.30*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*12.17*/(${"\"" * 3}margin: 0;
      |                padding: 0;
      |                font-family: Monaco, 'Lucida Console', monospace;
      |                background: #ECECEC;
      |            ${"\"" * 3}),format.raw/*16.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*16.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*17.13*/(${"\"" * 3}h1 ${"\"" * 3}),format.raw/*17.16*/(${"\"" * 3}{${"\"" * 3}),format.raw/*17.17*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*18.17*/(${"\"" * 3}margin: 0;
      |                background: #333;
      |                padding: 20px 45px;
      |                color: #fff;
      |                text-shadow: 1px 1px 1px rgba(0,0,0,.3);
      |                border-bottom: 1px solid #111;
      |                font-size: 28px;
      |            ${"\"" * 3}),format.raw/*25.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*25.14*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*26.13*/(${"\"" * 3}p#detail ${"\"" * 3}),format.raw/*26.22*/(${"\"" * 3}{${"\"" * 3}),format.raw/*26.23*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*27.17*/(${"\"" * 3}margin: 0;
      |                padding: 15px 45px;
      |                background: #888;
      |                border-top: 4px solid #666;
      |                color: #111;
      |                text-shadow: 1px 1px 1px rgba(255,255,255,.3);
      |                font-size: 14px;
      |                border-bottom: 1px solid #333;
      |            ${"\"" * 3}),format.raw/*35.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*35.14*/(${"\"" * 3}
      |        ${"\"" * 3})))}),format.raw/*36.10*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*37.5*/(${"\"" * 3}</head>
      |    <body>
      |        <h1>Unauthorized</h1>
      |        <p id="detail">
      |            You must be authenticated to access this page.
      |        </p>
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply()(request)
      |
      |  def f:(() => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = () => (request) => apply()(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/defaultpages/unauthorized.scala.html
      |                  HASH: b2e0704e0cb91a4d3962c30ad1c7fdc27fa7a860
      |                  MATRIX: 697->59|839->108|866->109|982->198|996->203|1053->251|1093->253|1134->266|1178->282|1207->283|1252->300|1434->454|1463->455|1504->468|1535->471|1564->472|1609->489|1896->748|1925->749|1966->762|2003->771|2032->772|2077->789|2414->1098|2443->1099|2484->1109|2516->1114
      |                  LINES: 17->4|22->5|23->6|27->10|27->10|27->10|27->10|28->11|28->11|28->11|29->12|33->16|33->16|34->17|34->17|34->17|35->18|42->25|42->25|43->26|43->26|43->26|44->27|52->35|52->35|53->36|54->37
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/checkbox.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object checkbox extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML input checkbox.
      | *
      | * Example:
      | * {{{
      | * @checkbox(field = myForm("done"))
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra HTML attributes ('''id''' and '''label''' are 2 special arguments).
      | * @param handler The field constructor.
      | * @param messages the provider of messages
      | */
      |  def apply/*14.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |def /*16.2*/boxValue/*16.10*/ = {{ args.toMap.get(Symbol("value")).getOrElse("true") }};
      |Seq[Any](format.raw/*15.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*16.67*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*17.2*/input(field, args:_*)/*17.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*17.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*18.5*/(${"\"" * 3}<input type="checkbox" id=${"\"" * 3}"),_display_(/*18.33*/id),format.raw/*18.35*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*18.44*/name),format.raw/*18.48*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*18.58*/boxValue),format.raw/*18.66*/(${"\"" * 3}" ${"\"" * 3}),_display_(if(value == Some(boxValue))/*18.96*/{_display_(Seq[Any](format.raw/*18.97*/(${"\"" * 3}checked="checked${"\"" * 3}")))} else {null} ),format.raw/*18.115*/(${"\"" * 3} ${"\"" * 3}),_display_(/*18.117*/toHtmlArgs(htmlArgs.view.filterKeys(_ != Symbol("value")).toMap)),format.raw/*18.181*/(${"\"" * 3}/>
      |    <span>${"\"" * 3}),_display_(/*19.12*/translate(args.toMap.get(Symbol("_text")))),format.raw/*19.54*/(${"\"" * 3}</span>
      |${"\"" * 3})))}),format.raw/*20.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/checkbox.scala.html
      |                  HASH: c743181eae7f035a8249c649eede8c52cc53029e
      |                  MATRIX: 1055->327|1261->457|1278->465|1365->455|1394->522|1422->524|1452->545|1523->578|1555->583|1610->611|1633->613|1669->622|1694->626|1731->636|1760->644|1817->674|1856->675|1919->693|1949->695|2035->759|2076->773|2139->815|2178->824
      |                  LINES: 28->14|32->16|32->16|33->15|34->16|35->17|35->17|35->17|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|36->18|37->19|37->19|38->20
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/checkbox.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object checkbox extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML input checkbox.
      | *
      | * Example:
      | * {{{
      | * @checkbox(field = myForm("done"))
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra HTML attributes ('''id''' and '''label''' are 2 special arguments).
      | * @param handler The field constructor.
      | * @param messages the provider of messages
      | */
      |  def apply/*14.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |def /*16.2*/boxValue/*16.10*/ = {{ args.toMap.get(Symbol("value")).getOrElse("true") }};
      |Seq[Any](format.raw/*15.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*16.67*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*17.2*/input(field, args:_*)/*17.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*17.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*18.5*/(${"\"" * 3}<input type="checkbox" id=${"\"" * 3}"),_display_(/*18.33*/id),format.raw/*18.35*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*18.44*/name),format.raw/*18.48*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*18.58*/boxValue),format.raw/*18.66*/(${"\"" * 3}" ${"\"" * 3}),_display_(if(value == Some(boxValue))/*18.96*/{_display_(Seq[Any](format.raw/*18.97*/(${"\"" * 3}checked="checked${"\"" * 3}")))} else {null} ),format.raw/*18.115*/(${"\"" * 3} ${"\"" * 3}),_display_(/*18.117*/toHtmlArgs(htmlArgs.view.filterKeys(_ != Symbol("value")).toMap)),format.raw/*18.181*/(${"\"" * 3}/>
      |    <span>${"\"" * 3}),_display_(/*19.12*/translate(args.toMap.get(Symbol("_text")))),format.raw/*19.54*/(${"\"" * 3}</span>
      |${"\"" * 3})))}),format.raw/*20.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/checkbox.scala.html
      |                  HASH: 581021bd2245c294dd874ef65aa603c6b00bab1d
      |                  MATRIX: 1017->327|1223->457|1240->465|1327->455|1356->522|1384->524|1414->545|1485->578|1517->583|1572->611|1595->613|1631->622|1656->626|1693->636|1722->644|1779->674|1818->675|1881->693|1911->695|1997->759|2038->773|2101->815|2140->824
      |                  LINES: 27->14|31->16|31->16|32->15|33->16|34->17|34->17|34->17|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|35->18|36->19|36->19|37->20
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/defaultFieldConstructor.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object defaultFieldConstructor extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[FieldElements,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default field constructor.
      | *
      | * It generates field as following:
      | * {{{
      | * <dl class="error">
      | *   <dt><label for="name">Your name:</label></dt>
      | *   <dd><input type="text" id="name" name="name"></dd>
      | *   <dd class="error">This field is required</dd>
      | *   <dd class="info">Required</dd>
      | * </dl>
      | * }}}
      | *
      | * @param el The field informations.
      | */
      |  def apply/*16.2*/(elements: FieldElements):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*17.1*/(${"\"" * 3}<dl class=${"\"" * 3}"),_display_(/*17.13*/elements/*17.21*/.args.get(Symbol("_class"))),format.raw/*17.48*/(${"\"" * 3} ${"\"" * 3}),_display_(if(elements.hasErrors)/*17.72*/ {_display_(Seq[Any](format.raw/*17.74*/(${"\"" * 3}error${"\"" * 3})))} else {null} ),format.raw/*17.80*/(${"\"" * 3}" id=${"\"" * 3}"),_display_(/*17.87*/elements/*17.95*/.args.get(Symbol("_id")).getOrElse(elements.id + "_field")),format.raw/*17.153*/(${"\"" * 3}">
      |    ${"\"" * 3}),_display_(if(elements.hasName)/*18.26*/ {_display_(Seq[Any](format.raw/*18.28*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*19.5*/(${"\"" * 3}<dt>${"\"" * 3}),_display_(/*19.10*/elements/*19.18*/.name),format.raw/*19.23*/(${"\"" * 3}</dt>
      |    ${"\"" * 3})))}else/*20.12*/{_display_(Seq[Any](format.raw/*20.13*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*21.5*/(${"\"" * 3}<dt><label for=${"\"" * 3}"),_display_(/*21.22*/elements/*21.30*/.id),format.raw/*21.33*/(${"\"" * 3}">${"\"" * 3}),_display_(/*21.36*/elements/*21.44*/.label),format.raw/*21.50*/(${"\"" * 3}</label></dt>
      |    ${"\"" * 3})))}),format.raw/*22.6*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*23.5*/(${"\"" * 3}<dd>${"\"" * 3}),_display_(/*23.10*/elements/*23.18*/.input),format.raw/*23.24*/(${"\"" * 3}</dd>
      |    ${"\"" * 3}),_display_(/*24.6*/elements/*24.14*/.errors.map/*24.25*/ { error =>_display_(Seq[Any](format.raw/*24.36*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*25.9*/(${"\"" * 3}<dd class="error">${"\"" * 3}),_display_(/*25.28*/error),format.raw/*25.33*/(${"\"" * 3}</dd>
      |    ${"\"" * 3})))}),format.raw/*26.6*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*27.6*/elements/*27.14*/.infos.map/*27.24*/ { info =>_display_(Seq[Any](format.raw/*27.34*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*28.9*/(${"\"" * 3}<dd class="info">${"\"" * 3}),_display_(/*28.27*/info),format.raw/*28.31*/(${"\"" * 3}</dd>
      |    ${"\"" * 3})))}),format.raw/*29.6*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*30.1*/(${"\"" * 3}</dl>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(elements:FieldElements): play.twirl.api.HtmlFormat.Appendable = apply(elements)
      |
      |  def f:((FieldElements) => play.twirl.api.HtmlFormat.Appendable) = (elements) => apply(elements)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/defaultFieldConstructor.scala.html
      |                  HASH: a36560039c31752a52beaf49419779d084b4d440
      |                  MATRIX: 1026->357|1146->383|1185->395|1202->403|1250->430|1301->454|1341->456|1391->462|1425->469|1442->477|1522->535|1577->563|1617->565|1649->570|1681->575|1698->583|1724->588|1758->605|1797->606|1829->611|1873->628|1890->636|1914->639|1944->642|1961->650|1988->656|2037->675|2069->680|2101->685|2118->693|2145->699|2182->710|2199->718|2219->729|2268->740|2304->749|2350->768|2376->773|2417->784|2449->790|2466->798|2485->808|2533->818|2569->827|2614->845|2639->849|2680->860|2708->861
      |                  LINES: 30->16|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|35->17|36->18|36->18|37->19|37->19|37->19|37->19|38->20|38->20|39->21|39->21|39->21|39->21|39->21|39->21|39->21|40->22|41->23|41->23|41->23|41->23|42->24|42->24|42->24|42->24|43->25|43->25|43->25|44->26|45->27|45->27|45->27|45->27|46->28|46->28|46->28|47->29|48->30
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/defaultFieldConstructor.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object defaultFieldConstructor extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[FieldElements,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Default field constructor.
      | *
      | * It generates field as following:
      | * {{{
      | * <dl class="error">
      | *   <dt><label for="name">Your name:</label></dt>
      | *   <dd><input type="text" id="name" name="name"></dd>
      | *   <dd class="error">This field is required</dd>
      | *   <dd class="info">Required</dd>
      | * </dl>
      | * }}}
      | *
      | * @param el The field informations.
      | */
      |  def apply/*16.2*/(elements: FieldElements):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*17.1*/(${"\"" * 3}<dl class=${"\"" * 3}"),_display_(/*17.13*/elements/*17.21*/.args.get(Symbol("_class"))),format.raw/*17.48*/(${"\"" * 3} ${"\"" * 3}),_display_(if(elements.hasErrors)/*17.72*/ {_display_(Seq[Any](format.raw/*17.74*/(${"\"" * 3}error${"\"" * 3})))} else {null} ),format.raw/*17.80*/(${"\"" * 3}" id=${"\"" * 3}"),_display_(/*17.87*/elements/*17.95*/.args.get(Symbol("_id")).getOrElse(elements.id + "_field")),format.raw/*17.153*/(${"\"" * 3}">
      |    ${"\"" * 3}),_display_(if(elements.hasName)/*18.26*/ {_display_(Seq[Any](format.raw/*18.28*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*19.5*/(${"\"" * 3}<dt>${"\"" * 3}),_display_(/*19.10*/elements/*19.18*/.name),format.raw/*19.23*/(${"\"" * 3}</dt>
      |    ${"\"" * 3})))}else/*20.12*/{_display_(Seq[Any](format.raw/*20.13*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*21.5*/(${"\"" * 3}<dt><label for=${"\"" * 3}"),_display_(/*21.22*/elements/*21.30*/.id),format.raw/*21.33*/(${"\"" * 3}">${"\"" * 3}),_display_(/*21.36*/elements/*21.44*/.label),format.raw/*21.50*/(${"\"" * 3}</label></dt>
      |    ${"\"" * 3})))}),format.raw/*22.6*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*23.5*/(${"\"" * 3}<dd>${"\"" * 3}),_display_(/*23.10*/elements/*23.18*/.input),format.raw/*23.24*/(${"\"" * 3}</dd>
      |    ${"\"" * 3}),_display_(/*24.6*/elements/*24.14*/.errors.map/*24.25*/ { error =>_display_(Seq[Any](format.raw/*24.36*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*25.9*/(${"\"" * 3}<dd class="error">${"\"" * 3}),_display_(/*25.28*/error),format.raw/*25.33*/(${"\"" * 3}</dd>
      |    ${"\"" * 3})))}),format.raw/*26.6*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*27.6*/elements/*27.14*/.infos.map/*27.24*/ { info =>_display_(Seq[Any](format.raw/*27.34*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*28.9*/(${"\"" * 3}<dd class="info">${"\"" * 3}),_display_(/*28.27*/info),format.raw/*28.31*/(${"\"" * 3}</dd>
      |    ${"\"" * 3})))}),format.raw/*29.6*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*30.1*/(${"\"" * 3}</dl>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(elements:FieldElements): play.twirl.api.HtmlFormat.Appendable = apply(elements)
      |
      |  def f:((FieldElements) => play.twirl.api.HtmlFormat.Appendable) = (elements) => apply(elements)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/defaultFieldConstructor.scala.html
      |                  HASH: 429e6678ce203edf83d5fa63bd29e128797c3661
      |                  MATRIX: 988->357|1108->383|1147->395|1164->403|1212->430|1263->454|1303->456|1353->462|1387->469|1404->477|1484->535|1539->563|1579->565|1611->570|1643->575|1660->583|1686->588|1720->605|1759->606|1791->611|1835->628|1852->636|1876->639|1906->642|1923->650|1950->656|1999->675|2031->680|2063->685|2080->693|2107->699|2144->710|2161->718|2181->729|2230->740|2266->749|2312->768|2338->773|2379->784|2411->790|2428->798|2447->808|2495->818|2531->827|2576->845|2601->849|2642->860|2670->861
      |                  LINES: 29->16|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|34->17|35->18|35->18|36->19|36->19|36->19|36->19|37->20|37->20|38->21|38->21|38->21|38->21|38->21|38->21|38->21|39->22|40->23|40->23|40->23|40->23|41->24|41->24|41->24|41->24|42->25|42->25|42->25|43->26|44->27|44->27|44->27|44->27|45->28|45->28|45->28|46->29|47->30
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/form.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object form extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[play.api.mvc.Call,Array[(Symbol,String)],Html,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML form.
      | *
      | * Example:
      | * {{{
      | * @form(action = routes.Users.submit, args = Symbol("class") -> "myForm") {
      | *   ...
      | * }
      | * }}}
      | *
      | * @param action The submit action.
      | * @param args Set of extra HTML attributes.
      | * @param body The form body.
      | */
      |  def apply/*15.2*/(action: play.api.mvc.Call, args: (Symbol,String)*)(body: => Html):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*15.68*/(${"\"" * 3} 
      |${"\"" * 3}),format.raw/*16.1*/(${"\"" * 3}<form action=${"\"" * 3}"),_display_(/*16.16*/action/*16.22*/.path),format.raw/*16.27*/(${"\"" * 3}" method=${"\"" * 3}"),_display_(/*16.38*/action/*16.44*/.method),format.raw/*16.51*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*16.54*/toHtmlArgs(args.toMap)),format.raw/*16.76*/(${"\"" * 3}>
      |    ${"\"" * 3}),_display_(/*17.6*/body),format.raw/*17.10*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*18.1*/(${"\"" * 3}</form>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(action:play.api.mvc.Call,args:Array[(Symbol,String)],body:Html): play.twirl.api.HtmlFormat.Appendable = apply(action,args.toIndexedSeq*)(body)
      |
      |  def f:((play.api.mvc.Call,Array[(Symbol,String)]) => (=> Html) => play.twirl.api.HtmlFormat.Appendable) = (action,args) => (body) => apply(action,args.toIndexedSeq*)(body)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/form.scala.html
      |                  HASH: f9543e0bd166d58eadff2c1e3f467406deb338c3
      |                  MATRIX: 951->269|1113->335|1142->337|1184->352|1199->358|1225->363|1263->374|1278->380|1306->387|1336->390|1379->412|1412->419|1437->423|1465->424
      |                  LINES: 29->15|34->15|35->16|35->16|35->16|35->16|35->16|35->16|35->16|35->16|35->16|36->17|36->17|37->18
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/form.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object form extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[play.api.mvc.Call,Array[(Symbol,String)],Html,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML form.
      | *
      | * Example:
      | * {{{
      | * @form(action = routes.Users.submit, args = Symbol("class") -> "myForm") {
      | *   ...
      | * }
      | * }}}
      | *
      | * @param action The submit action.
      | * @param args Set of extra HTML attributes.
      | * @param body The form body.
      | */
      |  def apply/*15.2*/(action: play.api.mvc.Call, args: (Symbol,String)*)(body: => Html):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*15.68*/(${"\"" * 3} 
      |${"\"" * 3}),format.raw/*16.1*/(${"\"" * 3}<form action=${"\"" * 3}"),_display_(/*16.16*/action/*16.22*/.path),format.raw/*16.27*/(${"\"" * 3}" method=${"\"" * 3}"),_display_(/*16.38*/action/*16.44*/.method),format.raw/*16.51*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*16.54*/toHtmlArgs(args.toMap)),format.raw/*16.76*/(${"\"" * 3}>
      |    ${"\"" * 3}),_display_(/*17.6*/body),format.raw/*17.10*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*18.1*/(${"\"" * 3}</form>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(action:play.api.mvc.Call,args:Array[(Symbol,String)],body:Html): play.twirl.api.HtmlFormat.Appendable = apply(action,args.toIndexedSeq: _*)(body)
      |
      |  def f:((play.api.mvc.Call,Array[(Symbol,String)]) => (=> Html) => play.twirl.api.HtmlFormat.Appendable) = (action,args) => (body) => apply(action,args.toIndexedSeq: _*)(body)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/form.scala.html
      |                  HASH: 36f116b71abe2cfe38737fec8a9f3599e503e526
      |                  MATRIX: 913->269|1075->335|1104->337|1146->352|1161->358|1187->363|1225->374|1240->380|1268->387|1298->390|1341->412|1374->419|1399->423|1427->424
      |                  LINES: 28->15|33->15|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|35->17|35->17|36->18
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/input.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object input extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Array[(Symbol, Any)],(String, String, Option[String], Map[Symbol,Any]) => Html,FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Prepare a generic HTML input.
      | */
      |  def apply/*4.2*/(field: play.api.data.Field, args: (Symbol, Any)* )(inputDef: (String, String, Option[String], Map[Symbol,Any]) => Html)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |def /*5.2*/id/*5.4*/ = {{ args.toMap.get(Symbol("id")).map(_.toString).getOrElse(field.id) }};
      |Seq[Any](format.raw/*5.76*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*6.2*/handler(
      |    FieldElements(
      |        id,
      |        field,
      |        inputDef(id, field.name, field.value, args.filter(arg => !arg._1.name.startsWith("_") && arg._1 != Symbol("id")).toMap),
      |        args.toMap,
      |        messages
      |    )
      |)),format.raw/*14.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol, Any)],inputDef:(String, String, Option[String], Map[Symbol,Any]) => Html,handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq*)(inputDef)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol, Any)]) => ((String, String, Option[String], Map[Symbol,Any]) => Html) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (inputDef) => (handler,messages) => apply(field,args.toIndexedSeq*)(inputDef)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/input.scala.html
      |                  HASH: 835c4c6479e916e096883b01008931708bcf599e
      |                  MATRIX: 825->42|1101->242|1110->244|1212->316|1239->318|1487->546
      |                  LINES: 18->4|22->5|22->5|23->5|24->6|32->14
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/input.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object input extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Array[(Symbol, Any)],(String, String, Option[String], Map[Symbol,Any]) => Html,FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Prepare a generic HTML input.
      | */
      |  def apply/*4.2*/(field: play.api.data.Field, args: (Symbol, Any)* )(inputDef: (String, String, Option[String], Map[Symbol,Any]) => Html)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |def /*5.2*/id/*5.4*/ = {{ args.toMap.get(Symbol("id")).map(_.toString).getOrElse(field.id) }};
      |Seq[Any](format.raw/*5.76*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*6.2*/handler(
      |    FieldElements(
      |        id,
      |        field,
      |        inputDef(id, field.name, field.value, args.filter(arg => !arg._1.name.startsWith("_") && arg._1 != Symbol("id")).toMap),
      |        args.toMap,
      |        messages
      |    )
      |)),format.raw/*14.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol, Any)],inputDef:(String, String, Option[String], Map[Symbol,Any]) => Html,handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(inputDef)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol, Any)]) => ((String, String, Option[String], Map[Symbol,Any]) => Html) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (inputDef) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(inputDef)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/input.scala.html
      |                  HASH: 808247a3df7f25487ad378583f5bbbbecb16024e
      |                  MATRIX: 787->42|1063->242|1072->244|1174->316|1201->318|1449->546
      |                  LINES: 17->4|21->5|21->5|22->5|23->6|31->14
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputCheckboxGroup.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputCheckboxGroup extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      |* Generate an HTML checkbox group
      |*
      |* Example:
      |* {{{
      |* @inputCheckboxGroup(
      |*           contactForm("hobbies"),
      |*           options = Seq("S" -> "Surfing", "R" -> "Running", "B" -> "Biking","P" -> "Paddling"),
      |*           Symbol("_label") -> "Hobbies",
      |*           Symbol("_error") -> contactForm("hobbies").error.map(_.withMessage("select one or more hobbies")))
      |*
      |* }}}
      |*
      |* @param field The form field.
      |* @param options Sequence of options as pairs of value and HTML
      |* @param args Set of extra HTML attributes.
      |* @param handler The field constructor.
      |*/
      |  def apply/*19.2*/(field: play.api.data.Field, options: Seq[(String,String)], args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*20.2*/input(field, args.map{ x => if(x._1 == Symbol("_label")) Symbol("_name") -> x._2 else x }:_*)/*20.95*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*20.128*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*21.3*/(${"\"" * 3}<span class="buttonset" id=${"\"" * 3}"),_display_(/*21.32*/id),format.raw/*21.34*/(${"\"" * 3}">
      |    ${"\"" * 3}),_display_(/*22.6*/defining(field.indexes.map( i => field("[%s]".format(i)).value ).flatten.toSet)/*22.85*/ { values =>_display_(Seq[Any](format.raw/*22.97*/(${"\"" * 3}
      |      ${"\"" * 3}),_display_(/*23.8*/options/*23.15*/.map/*23.19*/ { v =>_display_(Seq[Any](format.raw/*23.26*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*24.9*/(${"\"" * 3}<input type="checkbox" id=${"\"" * 3}"),_display_(/*24.37*/(id)),format.raw/*24.41*/(${"\"" * 3}_${"\"" * 3}),_display_(/*24.43*/v/*24.44*/._1),format.raw/*24.47*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*24.56*/{name + "[]"}),format.raw/*24.69*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*24.79*/v/*24.80*/._1),format.raw/*24.83*/(${"\"" * 3}" ${"\"" * 3}),_display_(if(values.contains(v._1))/*24.111*/{_display_(Seq[Any](format.raw/*24.112*/(${"\"" * 3}checked="checked${"\"" * 3}")))} else {null} ),format.raw/*24.130*/(${"\"" * 3} ${"\"" * 3}),_display_(/*24.132*/toHtmlArgs(htmlArgs)),format.raw/*24.152*/(${"\"" * 3}/>
      |        <label for=${"\"" * 3}"),_display_(/*25.22*/(id)),format.raw/*25.26*/(${"\"" * 3}_${"\"" * 3}),_display_(/*25.28*/v/*25.29*/._1),format.raw/*25.32*/(${"\"" * 3}">${"\"" * 3}),_display_(/*25.35*/v/*25.36*/._2),format.raw/*25.39*/(${"\"" * 3}</label>
      |      ${"\"" * 3})))}),format.raw/*26.8*/(${"\"" * 3}
      |    ${"\"" * 3})))}),format.raw/*27.6*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*28.3*/(${"\"" * 3}</span>
      |${"\"" * 3})))}),format.raw/*29.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputCheckboxGroup.scala.html
      |                  HASH: 7eb73d92914646458b05994b980556f03070739d
      |                  MATRIX: 1320->561|1573->721|1675->814|1747->847|1777->850|1833->879|1856->881|1890->889|1978->968|2028->980|2062->988|2078->995|2091->999|2136->1006|2172->1015|2227->1043|2252->1047|2281->1049|2291->1050|2315->1053|2351->1062|2385->1075|2422->1085|2432->1086|2456->1089|2512->1117|2552->1118|2615->1136|2645->1138|2687->1158|2738->1182|2763->1186|2792->1188|2802->1189|2826->1192|2856->1195|2866->1196|2890->1199|2936->1215|2972->1221|3002->1224|3041->1233
      |                  LINES: 33->19|38->20|38->20|38->20|39->21|39->21|39->21|40->22|40->22|40->22|41->23|41->23|41->23|41->23|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|43->25|43->25|43->25|43->25|43->25|43->25|43->25|43->25|44->26|45->27|46->28|47->29
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputCheckboxGroup.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputCheckboxGroup extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      |* Generate an HTML checkbox group
      |*
      |* Example:
      |* {{{
      |* @inputCheckboxGroup(
      |*           contactForm("hobbies"),
      |*           options = Seq("S" -> "Surfing", "R" -> "Running", "B" -> "Biking","P" -> "Paddling"),
      |*           Symbol("_label") -> "Hobbies",
      |*           Symbol("_error") -> contactForm("hobbies").error.map(_.withMessage("select one or more hobbies")))
      |*
      |* }}}
      |*
      |* @param field The form field.
      |* @param options Sequence of options as pairs of value and HTML
      |* @param args Set of extra HTML attributes.
      |* @param handler The field constructor.
      |*/
      |  def apply/*19.2*/(field: play.api.data.Field, options: Seq[(String,String)], args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*20.2*/input(field, args.map{ x => if(x._1 == Symbol("_label")) Symbol("_name") -> x._2 else x }:_*)/*20.95*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*20.128*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*21.3*/(${"\"" * 3}<span class="buttonset" id=${"\"" * 3}"),_display_(/*21.32*/id),format.raw/*21.34*/(${"\"" * 3}">
      |    ${"\"" * 3}),_display_(/*22.6*/defining(field.indexes.map( i => field("[%s]".format(i)).value ).flatten.toSet)/*22.85*/ { values =>_display_(Seq[Any](format.raw/*22.97*/(${"\"" * 3}
      |      ${"\"" * 3}),_display_(/*23.8*/options/*23.15*/.map/*23.19*/ { v =>_display_(Seq[Any](format.raw/*23.26*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*24.9*/(${"\"" * 3}<input type="checkbox" id=${"\"" * 3}"),_display_(/*24.37*/(id)),format.raw/*24.41*/(${"\"" * 3}_${"\"" * 3}),_display_(/*24.43*/v/*24.44*/._1),format.raw/*24.47*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*24.56*/{name + "[]"}),format.raw/*24.69*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*24.79*/v/*24.80*/._1),format.raw/*24.83*/(${"\"" * 3}" ${"\"" * 3}),_display_(if(values.contains(v._1))/*24.111*/{_display_(Seq[Any](format.raw/*24.112*/(${"\"" * 3}checked="checked${"\"" * 3}")))} else {null} ),format.raw/*24.130*/(${"\"" * 3} ${"\"" * 3}),_display_(/*24.132*/toHtmlArgs(htmlArgs)),format.raw/*24.152*/(${"\"" * 3}/>
      |        <label for=${"\"" * 3}"),_display_(/*25.22*/(id)),format.raw/*25.26*/(${"\"" * 3}_${"\"" * 3}),_display_(/*25.28*/v/*25.29*/._1),format.raw/*25.32*/(${"\"" * 3}">${"\"" * 3}),_display_(/*25.35*/v/*25.36*/._2),format.raw/*25.39*/(${"\"" * 3}</label>
      |      ${"\"" * 3})))}),format.raw/*26.8*/(${"\"" * 3}
      |    ${"\"" * 3})))}),format.raw/*27.6*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*28.3*/(${"\"" * 3}</span>
      |${"\"" * 3})))}),format.raw/*29.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputCheckboxGroup.scala.html
      |                  HASH: 506c64f467048a43060a6c60a8663a3b363a2404
      |                  MATRIX: 1282->561|1535->721|1637->814|1709->847|1739->850|1795->879|1818->881|1852->889|1940->968|1990->980|2024->988|2040->995|2053->999|2098->1006|2134->1015|2189->1043|2214->1047|2243->1049|2253->1050|2277->1053|2313->1062|2347->1075|2384->1085|2394->1086|2418->1089|2474->1117|2514->1118|2577->1136|2607->1138|2649->1158|2700->1182|2725->1186|2754->1188|2764->1189|2788->1192|2818->1195|2828->1196|2852->1199|2898->1215|2934->1221|2964->1224|3003->1233
      |                  LINES: 32->19|37->20|37->20|37->20|38->21|38->21|38->21|39->22|39->22|39->22|40->23|40->23|40->23|40->23|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|42->25|42->25|42->25|42->25|42->25|42->25|42->25|42->25|43->26|44->27|45->28|46->29
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputDate.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputDate extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML5 input date.
      | *
      | * Example:
      | * {{{
      | * @inputDate(field = myForm("releaseDate"), args = Symbol("size") -> 10)
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*14.2*/input(field, args:_*)/*14.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*14.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*15.5*/(${"\"" * 3}<input type="date" id=${"\"" * 3}"),_display_(/*15.29*/id),format.raw/*15.31*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*15.40*/name),format.raw/*15.44*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*15.54*/value),format.raw/*15.59*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*15.62*/toHtmlArgs(htmlArgs)),format.raw/*15.82*/(${"\"" * 3}/>
      |${"\"" * 3})))}),format.raw/*16.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputDate.scala.html
      |                  HASH: 8a45f2140d63a6e0ff827cb78030311ace6587ba
      |                  MATRIX: 990->261|1212->390|1242->411|1313->444|1345->449|1396->473|1419->475|1455->484|1480->488|1517->498|1543->503|1573->506|1614->526|1648->530
      |                  LINES: 27->13|32->14|32->14|32->14|33->15|33->15|33->15|33->15|33->15|33->15|33->15|33->15|33->15|34->16
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputDate.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputDate extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML5 input date.
      | *
      | * Example:
      | * {{{
      | * @inputDate(field = myForm("releaseDate"), args = Symbol("size") -> 10)
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*14.2*/input(field, args:_*)/*14.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*14.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*15.5*/(${"\"" * 3}<input type="date" id=${"\"" * 3}"),_display_(/*15.29*/id),format.raw/*15.31*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*15.40*/name),format.raw/*15.44*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*15.54*/value),format.raw/*15.59*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*15.62*/toHtmlArgs(htmlArgs)),format.raw/*15.82*/(${"\"" * 3}/>
      |${"\"" * 3})))}),format.raw/*16.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputDate.scala.html
      |                  HASH: bd824c92978b9cd69b63689918be910c9f4e1ca6
      |                  MATRIX: 952->261|1174->390|1204->411|1275->444|1307->449|1358->473|1381->475|1417->484|1442->488|1479->498|1505->503|1535->506|1576->526|1610->530
      |                  LINES: 26->13|31->14|31->14|31->14|32->15|32->15|32->15|32->15|32->15|32->15|32->15|32->15|32->15|33->16
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputFile.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputFile extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML input file.
      | *
      | * Example:
      | * {{{
      | * @inputFile(field = myForm("name"), args = Symbol("size") -> 10)
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*14.2*/input(field, args:_*)/*14.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*14.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*15.5*/(${"\"" * 3}<input type="file" id=${"\"" * 3}"),_display_(/*15.29*/id),format.raw/*15.31*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*15.40*/name),format.raw/*15.44*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*15.47*/toHtmlArgs(htmlArgs)),format.raw/*15.67*/(${"\"" * 3}/>
      |${"\"" * 3})))}),format.raw/*16.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputFile.scala.html
      |                  HASH: f4033a4d23c630cd71683e957fa67010deed2a97
      |                  MATRIX: 982->253|1204->382|1234->403|1305->436|1337->441|1388->465|1411->467|1447->476|1472->480|1502->483|1543->503|1577->507
      |                  LINES: 27->13|32->14|32->14|32->14|33->15|33->15|33->15|33->15|33->15|33->15|33->15|34->16
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputFile.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputFile extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML input file.
      | *
      | * Example:
      | * {{{
      | * @inputFile(field = myForm("name"), args = Symbol("size") -> 10)
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*14.2*/input(field, args:_*)/*14.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*14.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*15.5*/(${"\"" * 3}<input type="file" id=${"\"" * 3}"),_display_(/*15.29*/id),format.raw/*15.31*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*15.40*/name),format.raw/*15.44*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*15.47*/toHtmlArgs(htmlArgs)),format.raw/*15.67*/(${"\"" * 3}/>
      |${"\"" * 3})))}),format.raw/*16.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputFile.scala.html
      |                  HASH: e6276cd6bb62a52e41adb3b1fd2ed8ad7a2f6316
      |                  MATRIX: 944->253|1166->382|1196->403|1267->436|1299->441|1350->465|1373->467|1409->476|1434->480|1464->483|1505->503|1539->507
      |                  LINES: 26->13|31->14|31->14|31->14|32->15|32->15|32->15|32->15|32->15|32->15|32->15|33->16
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputPassword.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputPassword extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML input password.
      | *
      | * Example:
      | * {{{
      | * @inputPassword(field = myForm("password"), args = Symbol("size") -> 10)
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*14.2*/input(field, args:_*)/*14.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*14.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*15.5*/(${"\"" * 3}<input type="password" id=${"\"" * 3}"),_display_(/*15.33*/id),format.raw/*15.35*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*15.44*/name),format.raw/*15.48*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*15.51*/toHtmlArgs(htmlArgs)),format.raw/*15.71*/(${"\"" * 3}/>
      |${"\"" * 3})))}),format.raw/*16.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputPassword.scala.html
      |                  HASH: 299dafad423b651156810f70807ccb2a55e6a1bf
      |                  MATRIX: 998->265|1220->394|1250->415|1321->448|1353->453|1408->481|1431->483|1467->492|1492->496|1522->499|1563->519|1597->523
      |                  LINES: 27->13|32->14|32->14|32->14|33->15|33->15|33->15|33->15|33->15|33->15|33->15|34->16
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputPassword.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputPassword extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML input password.
      | *
      | * Example:
      | * {{{
      | * @inputPassword(field = myForm("password"), args = Symbol("size") -> 10)
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*14.2*/input(field, args:_*)/*14.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*14.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*15.5*/(${"\"" * 3}<input type="password" id=${"\"" * 3}"),_display_(/*15.33*/id),format.raw/*15.35*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*15.44*/name),format.raw/*15.48*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*15.51*/toHtmlArgs(htmlArgs)),format.raw/*15.71*/(${"\"" * 3}/>
      |${"\"" * 3})))}),format.raw/*16.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputPassword.scala.html
      |                  HASH: 4fd97a359efcb06843780d4ae0d01062f2161523
      |                  MATRIX: 960->265|1182->394|1212->415|1283->448|1315->453|1370->481|1393->483|1429->492|1454->496|1484->499|1525->519|1559->523
      |                  LINES: 26->13|31->14|31->14|31->14|32->15|32->15|32->15|32->15|32->15|32->15|32->15|33->16
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputRadioGroup.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputRadioGroup extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML radio group
      | *
      | * Example:
      | * {{{
      | * @inputRadioGroup(
      | *           contactForm("gender"),
      | *           options = Seq("M"->"Male","F"->"Female"),
      | *           Symbol("_label") -> "Gender",
      | *           Symbol("_error") -> contactForm("gender").error.map(_.withMessage("select gender")))
      | *
      | * }}}
      | *
      | * @param field The form field.
      | * @param options Seq of radio buttons encoded as value -> label
      | * @param args Set of extra HTML attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*19.2*/(field: play.api.data.Field, options: Seq[(String,String)], args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*20.2*/input(field, args.map{ x => if(x._1 == Symbol("_label")) Symbol("_name") -> x._2 else x }:_*)/*20.95*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*20.128*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*21.3*/(${"\"" * 3}<span class="buttonset" id=${"\"" * 3}"),_display_(/*21.32*/id),format.raw/*21.34*/(${"\"" * 3}">
      |    ${"\"" * 3}),_display_(/*22.6*/options/*22.13*/.map/*22.17*/ { v =>_display_(Seq[Any](format.raw/*22.24*/(${"\"" * 3}
      |      ${"\"" * 3}),format.raw/*23.7*/(${"\"" * 3}<input type="radio" id=${"\"" * 3}"),_display_(/*23.32*/(id)),format.raw/*23.36*/(${"\"" * 3}_${"\"" * 3}),_display_(/*23.38*/v/*23.39*/._1),format.raw/*23.42*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*23.51*/name),format.raw/*23.55*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*23.65*/v/*23.66*/._1),format.raw/*23.69*/(${"\"" * 3}" ${"\"" * 3}),_display_(if(value == Some(v._1))/*23.95*/{_display_(Seq[Any](format.raw/*23.96*/(${"\"" * 3}checked="checked${"\"" * 3}")))} else {null} ),format.raw/*23.114*/(${"\"" * 3} ${"\"" * 3}),_display_(/*23.116*/toHtmlArgs(htmlArgs)),format.raw/*23.136*/(${"\"" * 3}/>
      |      <label for=${"\"" * 3}"),_display_(/*24.20*/(id)),format.raw/*24.24*/(${"\"" * 3}_${"\"" * 3}),_display_(/*24.26*/v/*24.27*/._1),format.raw/*24.30*/(${"\"" * 3}">${"\"" * 3}),_display_(/*24.33*/v/*24.34*/._2),format.raw/*24.37*/(${"\"" * 3}</label>
      |    ${"\"" * 3})))}),format.raw/*25.6*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*26.3*/(${"\"" * 3}</span>
      |${"\"" * 3})))}),format.raw/*27.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputRadioGroup.scala.html
      |                  HASH: 32777da109601cfe3e205213f9d5d5494c119135
      |                  MATRIX: 1268->512|1521->672|1623->765|1695->798|1725->801|1781->830|1804->832|1838->840|1854->847|1867->851|1912->858|1946->865|1998->890|2023->894|2052->896|2062->897|2086->900|2122->909|2147->913|2184->923|2194->924|2218->927|2271->953|2310->954|2373->972|2403->974|2445->994|2494->1016|2519->1020|2548->1022|2558->1023|2582->1026|2612->1029|2622->1030|2646->1033|2690->1047|2720->1050|2759->1059
      |                  LINES: 33->19|38->20|38->20|38->20|39->21|39->21|39->21|40->22|40->22|40->22|40->22|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|41->23|42->24|42->24|42->24|42->24|42->24|42->24|42->24|42->24|43->25|44->26|45->27
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputRadioGroup.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputRadioGroup extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML radio group
      | *
      | * Example:
      | * {{{
      | * @inputRadioGroup(
      | *           contactForm("gender"),
      | *           options = Seq("M"->"Male","F"->"Female"),
      | *           Symbol("_label") -> "Gender",
      | *           Symbol("_error") -> contactForm("gender").error.map(_.withMessage("select gender")))
      | *
      | * }}}
      | *
      | * @param field The form field.
      | * @param options Seq of radio buttons encoded as value -> label
      | * @param args Set of extra HTML attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*19.2*/(field: play.api.data.Field, options: Seq[(String,String)], args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*20.2*/input(field, args.map{ x => if(x._1 == Symbol("_label")) Symbol("_name") -> x._2 else x }:_*)/*20.95*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*20.128*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*21.3*/(${"\"" * 3}<span class="buttonset" id=${"\"" * 3}"),_display_(/*21.32*/id),format.raw/*21.34*/(${"\"" * 3}">
      |    ${"\"" * 3}),_display_(/*22.6*/options/*22.13*/.map/*22.17*/ { v =>_display_(Seq[Any](format.raw/*22.24*/(${"\"" * 3}
      |      ${"\"" * 3}),format.raw/*23.7*/(${"\"" * 3}<input type="radio" id=${"\"" * 3}"),_display_(/*23.32*/(id)),format.raw/*23.36*/(${"\"" * 3}_${"\"" * 3}),_display_(/*23.38*/v/*23.39*/._1),format.raw/*23.42*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*23.51*/name),format.raw/*23.55*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*23.65*/v/*23.66*/._1),format.raw/*23.69*/(${"\"" * 3}" ${"\"" * 3}),_display_(if(value == Some(v._1))/*23.95*/{_display_(Seq[Any](format.raw/*23.96*/(${"\"" * 3}checked="checked${"\"" * 3}")))} else {null} ),format.raw/*23.114*/(${"\"" * 3} ${"\"" * 3}),_display_(/*23.116*/toHtmlArgs(htmlArgs)),format.raw/*23.136*/(${"\"" * 3}/>
      |      <label for=${"\"" * 3}"),_display_(/*24.20*/(id)),format.raw/*24.24*/(${"\"" * 3}_${"\"" * 3}),_display_(/*24.26*/v/*24.27*/._1),format.raw/*24.30*/(${"\"" * 3}">${"\"" * 3}),_display_(/*24.33*/v/*24.34*/._2),format.raw/*24.37*/(${"\"" * 3}</label>
      |    ${"\"" * 3})))}),format.raw/*25.6*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*26.3*/(${"\"" * 3}</span>
      |${"\"" * 3})))}),format.raw/*27.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputRadioGroup.scala.html
      |                  HASH: b9a825e67f7f15d7d26dfd3727682d1d5b2d5366
      |                  MATRIX: 1230->512|1483->672|1585->765|1657->798|1687->801|1743->830|1766->832|1800->840|1816->847|1829->851|1874->858|1908->865|1960->890|1985->894|2014->896|2024->897|2048->900|2084->909|2109->913|2146->923|2156->924|2180->927|2233->953|2272->954|2335->972|2365->974|2407->994|2456->1016|2481->1020|2510->1022|2520->1023|2544->1026|2574->1029|2584->1030|2608->1033|2652->1047|2682->1050|2721->1059
      |                  LINES: 32->19|37->20|37->20|37->20|38->21|38->21|38->21|39->22|39->22|39->22|39->22|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|40->23|41->24|41->24|41->24|41->24|41->24|41->24|41->24|41->24|42->25|43->26|44->27
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputText.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputText extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML input text.
      | *
      | * Example:
      | * {{{
      | * @inputText(field = myForm("name"), args = Symbol("size") -> 10, Symbol("placeholder") -> "Your name")
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |def /*14.2*/inputType/*14.11*/ = {{ args.toMap.get(Symbol("type")).map(_.toString).getOrElse("text") }};
      |Seq[Any](format.raw/*14.83*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*15.2*/input(field, args.filter(_._1 != Symbol("type")):_*)/*15.54*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*15.87*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*16.5*/(${"\"" * 3}<input type=${"\"" * 3}"),_display_(/*16.19*/inputType),format.raw/*16.28*/(${"\"" * 3}" id=${"\"" * 3}"),_display_(/*16.35*/id),format.raw/*16.37*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*16.46*/name),format.raw/*16.50*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*16.60*/value),format.raw/*16.65*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*16.68*/toHtmlArgs(htmlArgs)),format.raw/*16.88*/(${"\"" * 3}/>
      |${"\"" * 3})))}),format.raw/*17.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputText.scala.html
      |                  HASH: 6488ba3edfd2a3ce8ba6b549b8246eecbe8672c3
      |                  MATRIX: 1020->291|1226->420|1244->429|1347->501|1375->503|1436->555|1507->588|1539->593|1580->607|1610->616|1644->623|1667->625|1703->634|1728->638|1765->648|1791->653|1821->656|1862->676|1896->680
      |                  LINES: 27->13|31->14|31->14|32->14|33->15|33->15|33->15|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|34->16|35->17
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/inputText.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object inputText extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML input text.
      | *
      | * Example:
      | * {{{
      | * @inputText(field = myForm("name"), args = Symbol("size") -> 10, Symbol("placeholder") -> "Your name")
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |def /*14.2*/inputType/*14.11*/ = {{ args.toMap.get(Symbol("type")).map(_.toString).getOrElse("text") }};
      |Seq[Any](format.raw/*14.83*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*15.2*/input(field, args.filter(_._1 != Symbol("type")):_*)/*15.54*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*15.87*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*16.5*/(${"\"" * 3}<input type=${"\"" * 3}"),_display_(/*16.19*/inputType),format.raw/*16.28*/(${"\"" * 3}" id=${"\"" * 3}"),_display_(/*16.35*/id),format.raw/*16.37*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*16.46*/name),format.raw/*16.50*/(${"\"" * 3}" value=${"\"" * 3}"),_display_(/*16.60*/value),format.raw/*16.65*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*16.68*/toHtmlArgs(htmlArgs)),format.raw/*16.88*/(${"\"" * 3}/>
      |${"\"" * 3})))}),format.raw/*17.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/inputText.scala.html
      |                  HASH: 026cd2aaf712590b2f9820c1e3a0ef96fec7e301
      |                  MATRIX: 982->291|1188->420|1206->429|1309->501|1337->503|1398->555|1469->588|1501->593|1542->607|1572->616|1606->623|1629->625|1665->634|1690->638|1727->648|1753->653|1783->656|1824->676|1858->680
      |                  LINES: 26->13|30->14|30->14|31->14|32->15|32->15|32->15|33->16|33->16|33->16|33->16|33->16|33->16|33->16|33->16|33->16|33->16|33->16|34->17
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/javascriptRouter.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object javascriptRouter extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[String,Array[play.api.routing.JavaScriptReverseRoute],play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generates a Javascript object that lets you refer 
      | * to your application's routes in Javascript code
      | *
      | * Example:
      | * {{{
      | * @javascriptRouter("jsRoutes")(
      | *   routes.javascript.Users.list,
      | *   routes.javascript.Application.index
      | * )
      | * }}}
      | *
      | * You can access your routes in JavaScript without hardcoded URL's, e.g. assuming jQuery's ajax function:
      | * {{{
      | * $$.ajax(jsRoutes.controllers.Users.list()).done( /* */ ).fail( /* */ )
      | * }}}
      | * Each action in the generated object also has the following properties:
      | * * *type*: HTTP method
      | * * *url*: the url to be used
      | * 
      | * @param name The javascript object name.
      | * @param routes Set of routes to include in this javascript router.
      | */
      |  def apply/*24.2*/(name:String = "Router")(routes: play.api.routing.JavaScriptReverseRoute*)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*25.2*/script(Symbol("type") -> "text/javascript")/*25.45*/ {_display_(Seq[Any](format.raw/*25.47*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*26.6*/Html(play.api.routing.JavaScriptReverseRouter(name)(routes: _*).body.replace("/", "\\\\/"))),format.raw/*26.95*/(${"\"" * 3}
      |${"\"" * 3})))}))
      |      }
      |    }
      |  }
      |
      |  def render(name:String,routes:Array[play.api.routing.JavaScriptReverseRoute],request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(name)(routes.toIndexedSeq*)(request)
      |
      |  def f:((String) => (Array[play.api.routing.JavaScriptReverseRoute]) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (name) => (routes) => (request) => apply(name)(routes.toIndexedSeq*)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/javascriptRouter.scala.html
      |                  HASH: 16956caa59533c2f73184aa4952d46e21e78068c
      |                  MATRIX: 1430->701|1645->823|1697->866|1737->868|1769->874|1879->963
      |                  LINES: 38->24|43->25|43->25|43->25|44->26|44->26
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/javascriptRouter.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object javascriptRouter extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[String,Array[play.api.routing.JavaScriptReverseRoute],play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generates a Javascript object that lets you refer 
      | * to your application's routes in Javascript code
      | *
      | * Example:
      | * {{{
      | * @javascriptRouter("jsRoutes")(
      | *   routes.javascript.Users.list,
      | *   routes.javascript.Application.index
      | * )
      | * }}}
      | *
      | * You can access your routes in JavaScript without hardcoded URL's, e.g. assuming jQuery's ajax function:
      | * {{{
      | * $$.ajax(jsRoutes.controllers.Users.list()).done( /* */ ).fail( /* */ )
      | * }}}
      | * Each action in the generated object also has the following properties:
      | * * *type*: HTTP method
      | * * *url*: the url to be used
      | * 
      | * @param name The javascript object name.
      | * @param routes Set of routes to include in this javascript router.
      | */
      |  def apply/*24.2*/(name:String = "Router")(routes: play.api.routing.JavaScriptReverseRoute*)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*25.2*/script(Symbol("type") -> "text/javascript")/*25.45*/ {_display_(Seq[Any](format.raw/*25.47*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*26.6*/Html(play.api.routing.JavaScriptReverseRouter(name)(routes: _*).body.replace("/", "\\\\/"))),format.raw/*26.95*/(${"\"" * 3}
      |${"\"" * 3})))}))
      |      }
      |    }
      |  }
      |
      |  def render(name:String,routes:Array[play.api.routing.JavaScriptReverseRoute],request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(name)(routes.toIndexedSeq: _*)(request)
      |
      |  def f:((String) => (Array[play.api.routing.JavaScriptReverseRoute]) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (name) => (routes) => (request) => apply(name)(routes.toIndexedSeq: _*)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/javascriptRouter.scala.html
      |                  HASH: 13f437cea1e3ade0e8935851b91ebec26647ac84
      |                  MATRIX: 1392->701|1607->823|1659->866|1699->868|1731->874|1841->963
      |                  LINES: 37->24|42->25|42->25|42->25|43->26|43->26
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/jsloader.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object jsloader extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /* TODO: Remove the dependency to jQuery? */
      |  def apply/*7.2*/()(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*8.1*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*9.2*/script(Symbol("type") -> "text/javascript")/*9.45*/ {_display_(Seq[Any](format.raw/*9.47*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*10.1*/(${"\"" * 3}var require = function(moduleName) ${"\"" * 3}),format.raw/*10.36*/(${"\"" * 3}{${"\"" * 3}),format.raw/*10.37*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*11.3*/(${"\"" * 3}var body = "";
      |  $$.ajax(${"\"" * 3}),format.raw/*12.10*/(${"\"" * 3}{${"\"" * 3}),format.raw/*12.11*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*13.5*/(${"\"" * 3}url: "/assets/javascripts/" + moduleName + ".js",
      |    dataType: "text", async: false,
      |    success: function(result) ${"\"" * 3}),format.raw/*15.31*/(${"\"" * 3}{${"\"" * 3}),format.raw/*15.32*/(${"\"" * 3} ${"\"" * 3}),format.raw/*15.33*/(${"\"" * 3}body = result; ${"\"" * 3}),format.raw/*15.48*/(${"\"" * 3}}${"\"" * 3}),format.raw/*15.49*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*16.3*/(${"\"" * 3}}${"\"" * 3}),format.raw/*16.4*/(${"\"" * 3});
      |  body = "var exports = ${"\"" * 3}),format.raw/*17.25*/(${"\"" * 3}{${"\"" * 3}),format.raw/*17.26*/(${"\"" * 3}}${"\"" * 3}),format.raw/*17.27*/(${"\"" * 3};\\n" + body + "\\nreturn exports;";
      |  var fnct = new Function("module", "exports", body);
      |  return fnct();
      |${"\"" * 3}),format.raw/*20.1*/(${"\"" * 3}}${"\"" * 3}),format.raw/*20.2*/(${"\"" * 3}
      |${"\"" * 3})))}))
      |      }
      |    }
      |  }
      |
      |  def render(request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply()(request)
      |
      |  def f:(() => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = () => (request) => apply()(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/jsloader.scala.html
      |                  HASH: 8d1426f98e0ec140d1cf58953e4b11c55078f8a2
      |                  MATRIX: 712->188|854->237|881->239|932->282|971->284|999->285|1062->320|1091->321|1121->324|1173->348|1202->349|1234->354|1378->470|1407->471|1436->472|1479->487|1508->488|1538->491|1566->492|1621->519|1650->520|1679->521|1812->627|1840->628
      |                  LINES: 16->7|21->8|22->9|22->9|22->9|23->10|23->10|23->10|24->11|25->12|25->12|26->13|28->15|28->15|28->15|28->15|28->15|29->16|29->16|30->17|30->17|30->17|33->20|33->20
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/jsloader.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object jsloader extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template1[play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /* TODO: Remove the dependency to jQuery? */
      |  def apply/*7.2*/()(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*8.1*/(${"\"" * 3}
      |${"\"" * 3}),_display_(/*9.2*/script(Symbol("type") -> "text/javascript")/*9.45*/ {_display_(Seq[Any](format.raw/*9.47*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*10.1*/(${"\"" * 3}var require = function(moduleName) ${"\"" * 3}),format.raw/*10.36*/(${"\"" * 3}{${"\"" * 3}),format.raw/*10.37*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*11.3*/(${"\"" * 3}var body = "";
      |  $$.ajax(${"\"" * 3}),format.raw/*12.10*/(${"\"" * 3}{${"\"" * 3}),format.raw/*12.11*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*13.5*/(${"\"" * 3}url: "/assets/javascripts/" + moduleName + ".js",
      |    dataType: "text", async: false,
      |    success: function(result) ${"\"" * 3}),format.raw/*15.31*/(${"\"" * 3}{${"\"" * 3}),format.raw/*15.32*/(${"\"" * 3} ${"\"" * 3}),format.raw/*15.33*/(${"\"" * 3}body = result; ${"\"" * 3}),format.raw/*15.48*/(${"\"" * 3}}${"\"" * 3}),format.raw/*15.49*/(${"\"" * 3}
      |  ${"\"" * 3}),format.raw/*16.3*/(${"\"" * 3}}${"\"" * 3}),format.raw/*16.4*/(${"\"" * 3});
      |  body = "var exports = ${"\"" * 3}),format.raw/*17.25*/(${"\"" * 3}{${"\"" * 3}),format.raw/*17.26*/(${"\"" * 3}}${"\"" * 3}),format.raw/*17.27*/(${"\"" * 3};\\n" + body + "\\nreturn exports;";
      |  var fnct = new Function("module", "exports", body);
      |  return fnct();
      |${"\"" * 3}),format.raw/*20.1*/(${"\"" * 3}}${"\"" * 3}),format.raw/*20.2*/(${"\"" * 3}
      |${"\"" * 3})))}))
      |      }
      |    }
      |  }
      |
      |  def render(request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply()(request)
      |
      |  def f:(() => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = () => (request) => apply()(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/jsloader.scala.html
      |                  HASH: 6c7cd39ab3c5698eda01d1b1a009d36a6c88bf7e
      |                  MATRIX: 674->188|816->237|843->239|894->282|933->284|961->285|1024->320|1053->321|1083->324|1135->348|1164->349|1196->354|1340->470|1369->471|1398->472|1441->487|1470->488|1500->491|1528->492|1583->519|1612->520|1641->521|1774->627|1802->628
      |                  LINES: 15->7|20->8|21->9|21->9|21->9|22->10|22->10|22->10|23->11|24->12|24->12|25->13|27->15|27->15|27->15|27->15|27->15|28->16|28->16|29->17|29->17|29->17|32->20|32->20
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/requireJs.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object requireJs extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[String,String,Boolean,String,String,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * RequireJS Javascript module loader.
      | *
      | * Example:
      | * {{{
      | * @requireJs(core = routes.Assets.at("javascripts/require.js").url, module = routes.Assets.at("javascripts/main").url, isProd = true)
      | * }}}
      | *
      | * @param module Javascript module in question.
      | * @param core Reference to require.js.
      | * @param isProd true if the javascript should be minified, false otherwise.
      | * @param productionFolderPrefix Prefix of Javascript production folder, default "-min".
      | * @param folder Javascript folder, default "javascripts".
      | */
      |  def apply/*16.2*/(module: String, core: String, isProd: Boolean, productionFolderPrefix: String = "-min", folder: String = "javascripts"):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*17.1*/(${"\"" * 3}<script type="text/javascript" data-main=${"\"" * 3}"),_display_(/*17.44*/{if(isProd) module.replace(folder,folder+productionFolderPrefix) else module}),format.raw/*17.121*/(${"\"" * 3}" src=${"\"" * 3}"),_display_(/*17.129*/core),format.raw/*17.133*/(${"\"" * 3}"></script>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(module:String,core:String,isProd:Boolean,productionFolderPrefix:String,folder:String): play.twirl.api.HtmlFormat.Appendable = apply(module,core,isProd,productionFolderPrefix,folder)
      |
      |  def f:((String,String,Boolean,String,String) => play.twirl.api.HtmlFormat.Appendable) = (module,core,isProd,productionFolderPrefix,folder) => apply(module,core,isProd,productionFolderPrefix,folder)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/requireJs.scala.html
      |                  HASH: f1f788871d0682d1402e6e85b07fd768d93bc2b4
      |                  MATRIX: 1205->529|1420->650|1490->693|1589->770|1625->778|1651->782
      |                  LINES: 29->16|34->17|34->17|34->17|34->17|34->17
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/requireJs.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object requireJs extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[String,String,Boolean,String,String,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * RequireJS Javascript module loader.
      | *
      | * Example:
      | * {{{
      | * @requireJs(core = routes.Assets.at("javascripts/require.js").url, module = routes.Assets.at("javascripts/main").url, isProd = true)
      | * }}}
      | *
      | * @param module Javascript module in question.
      | * @param core Reference to require.js.
      | * @param isProd true if the javascript should be minified, false otherwise.
      | * @param productionFolderPrefix Prefix of Javascript production folder, default "-min".
      | * @param folder Javascript folder, default "javascripts".
      | */
      |  def apply/*16.2*/(module: String, core: String, isProd: Boolean, productionFolderPrefix: String = "-min", folder: String = "javascripts"):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*17.1*/(${"\"" * 3}<script type="text/javascript" data-main=${"\"" * 3}"),_display_(/*17.44*/{if(isProd) module.replace(folder,folder+productionFolderPrefix) else module}),format.raw/*17.121*/(${"\"" * 3}" src=${"\"" * 3}"),_display_(/*17.129*/core),format.raw/*17.133*/(${"\"" * 3}"></script>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(module:String,core:String,isProd:Boolean,productionFolderPrefix:String,folder:String): play.twirl.api.HtmlFormat.Appendable = apply(module,core,isProd,productionFolderPrefix,folder)
      |
      |  def f:((String,String,Boolean,String,String) => play.twirl.api.HtmlFormat.Appendable) = (module,core,isProd,productionFolderPrefix,folder) => apply(module,core,isProd,productionFolderPrefix,folder)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/requireJs.scala.html
      |                  HASH: 9c592686bc2b1e73611309bba7097889155b3907
      |                  MATRIX: 1167->529|1382->650|1452->693|1551->770|1587->778|1613->782
      |                  LINES: 28->16|33->17|33->17|33->17|33->17|33->17
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/script.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object script extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[Array[(Symbol,String)],Html,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      |* Generate an inline script with CSP nonce.
      |*
      |* Example:
      |* {{{
      |* @script(args = Symbol("type") -> "text/javascript") {
      |*   ...
      |* }
      |* }}}
      |*
      |* See <a href="https://www.w3.org/TR/2016/REC-html51-20161101/semantics-scripting.html">Scripting</a>
      |* for more information.
      |*
      |* @param args Set of extra HTML attributes.
      |* @param body The script body.
      |*/
      |  def apply/*17.2*/(args: (Symbol,String)*)(body: => Html)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*18.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*19.1*/(${"\"" * 3}<script ${"\"" * 3}),_display_(/*19.10*/{CSPNonce.attr}),format.raw/*19.25*/(${"\"" * 3} ${"\"" * 3}),_display_(/*19.27*/toHtmlArgs(args.toMap)),format.raw/*19.49*/(${"\"" * 3}>${"\"" * 3}),_display_(/*19.51*/body),format.raw/*19.55*/(${"\"" * 3}</script>${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(args:Array[(Symbol,String)],body:Html,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(args.toIndexedSeq*)(body)(request)
      |
      |  def f:((Array[(Symbol,String)]) => (=> Html) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (args) => (body) => (request) => apply(args.toIndexedSeq*)(body)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/script.scala.html
      |                  HASH: 6236c233b0ab7d8f821608cddd082020a50419d2
      |                  MATRIX: 1043->350|1223->436|1251->437|1287->446|1323->461|1352->463|1395->485|1424->487|1449->491
      |                  LINES: 31->17|36->18|37->19|37->19|37->19|37->19|37->19|37->19|37->19
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/script.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object script extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[Array[(Symbol,String)],Html,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      |* Generate an inline script with CSP nonce.
      |*
      |* Example:
      |* {{{
      |* @script(args = Symbol("type") -> "text/javascript") {
      |*   ...
      |* }
      |* }}}
      |*
      |* See <a href="https://www.w3.org/TR/2016/REC-html51-20161101/semantics-scripting.html">Scripting</a>
      |* for more information.
      |*
      |* @param args Set of extra HTML attributes.
      |* @param body The script body.
      |*/
      |  def apply/*17.2*/(args: (Symbol,String)*)(body: => Html)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*18.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*19.1*/(${"\"" * 3}<script ${"\"" * 3}),_display_(/*19.10*/{CSPNonce.attr}),format.raw/*19.25*/(${"\"" * 3} ${"\"" * 3}),_display_(/*19.27*/toHtmlArgs(args.toMap)),format.raw/*19.49*/(${"\"" * 3}>${"\"" * 3}),_display_(/*19.51*/body),format.raw/*19.55*/(${"\"" * 3}</script>${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(args:Array[(Symbol,String)],body:Html,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(args.toIndexedSeq: _*)(body)(request)
      |
      |  def f:((Array[(Symbol,String)]) => (=> Html) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (args) => (body) => (request) => apply(args.toIndexedSeq: _*)(body)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/script.scala.html
      |                  HASH: 5d104820328161da5dce8671010304183b6e4b2a
      |                  MATRIX: 1005->350|1185->436|1213->437|1249->446|1285->461|1314->463|1357->485|1386->487|1411->491
      |                  LINES: 30->17|35->18|36->19|36->19|36->19|36->19|36->19|36->19|36->19
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/select.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object select extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML select.
      | *
      | * Example:
      | * {{{
      | * @select(
      | *   field = myForm("mySelect"),
      | *   options = Seq(
      | *     "Foo" -> "foo text",
      | *     "Bar" -> "bar text",
      | *     "Baz" -> "baz text"
      | *    ),
      | *   Symbol("_default") -> "Choose One",
      | *   Symbol("_disabled") -> Seq("FooKey", "BazKey")
      | *   Symbol("cust_att_name") -> "cust_att_value"
      | * )
      | * }}}
      | *
      | * @param field The form field.
      | * @param options Sequence of options as pairs of value and HTML.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*24.2*/(field: play.api.data.Field, options: Seq[(String,String)], args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*25.2*/input(field, args:_*)/*25.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*25.56*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*26.6*/defining( if( htmlArgs.contains(Symbol("multiple")) ) "%s[]".format(name) else name )/*26.91*/ { selectName =>_display_(Seq[Any](format.raw/*26.107*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*27.6*/defining( field.indexes.nonEmpty && htmlArgs.contains(Symbol("multiple")) match {
      |            case true => field.indexes.map( i => field("[%s]".format(i)).value ).flatten.toSet
      |            case _ => field.value.toSet
      |    })/*30.7*/{ selectedValues =>_display_(Seq[Any](format.raw/*30.26*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*31.9*/(${"\"" * 3}<select id=${"\"" * 3}"),_display_(/*31.22*/id),format.raw/*31.24*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*31.33*/selectName),format.raw/*31.43*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*31.46*/toHtmlArgs(htmlArgs)),format.raw/*31.66*/(${"\"" * 3}>
      |            ${"\"" * 3}),_display_(/*32.14*/args/*32.18*/.toMap.get(Symbol("_default")).map/*32.52*/ { defaultValue =>_display_(Seq[Any](format.raw/*32.70*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*33.17*/(${"\"" * 3}<option class="blank" value="">${"\"" * 3}),_display_(/*33.49*/translate(defaultValue)),format.raw/*33.72*/(${"\"" * 3}</option>
      |            ${"\"" * 3})))}),format.raw/*34.14*/(${"\"" * 3}
      |            ${"\"" * 3}),_display_(/*35.14*/options/*35.21*/.map/*35.25*/ { case (k, v) =>_display_(Seq[Any](format.raw/*35.42*/(${"\"" * 3}
      |                ${"\"" * 3}),_display_(/*36.18*/defining( selectedValues.contains(k) )/*36.56*/ { selected =>_display_(Seq[Any](format.raw/*36.70*/(${"\"" * 3}
      |                ${"\"" * 3}),_display_(/*37.18*/defining( args.toMap.get(Symbol("_disabled")).exists { case s: Seq[_] => s.asInstanceOf[Seq[String]].contains(k) })/*37.133*/{ disabled =>_display_(Seq[Any](format.raw/*37.146*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*38.17*/(${"\"" * 3}<option value=${"\"" * 3}"),_display_(/*38.33*/k),format.raw/*38.34*/(${"\"" * 3}${"\"" * 3}"),_display_(if(selected)/*38.48*/{_display_(Seq[Any](format.raw/*38.49*/(${"\"" * 3} ${"\"" * 3}),format.raw/*38.50*/(${"\"" * 3}selected="selected${"\"" * 3}")))} else {null} ),_display_(if(disabled)/*38.83*/{_display_(Seq[Any](format.raw/*38.84*/(${"\"" * 3} ${"\"" * 3}),format.raw/*38.85*/(${"\"" * 3}disabled${"\"" * 3})))} else {null} ),format.raw/*38.94*/(${"\"" * 3}>${"\"" * 3}),_display_(/*38.96*/v),format.raw/*38.97*/(${"\"" * 3}</option>
      |            ${"\"" * 3})))})))})))}),format.raw/*39.16*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*40.9*/(${"\"" * 3}</select>
      |    ${"\"" * 3})))})))}),format.raw/*41.7*/(${"\"" * 3}
      |${"\"" * 3})))}),format.raw/*42.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/select.scala.html
      |                  HASH: 521c401986b87e9e705c0a74690bbee0e80056dc
      |                  MATRIX: 1299->552|1552->712|1582->733|1653->766|1685->772|1779->857|1834->873|1866->879|2097->1102|2154->1121|2190->1130|2230->1143|2253->1145|2289->1154|2320->1164|2350->1167|2391->1187|2433->1202|2446->1206|2489->1240|2545->1258|2590->1275|2649->1307|2693->1330|2747->1353|2788->1367|2804->1374|2817->1378|2872->1395|2917->1413|2964->1451|3016->1465|3061->1483|3186->1598|3238->1611|3283->1628|3326->1644|3348->1645|3389->1659|3428->1660|3457->1661|3533->1694|3572->1695|3601->1696|3654->1705|3683->1707|3705->1708|3767->1733|3803->1742|3852->1758|3884->1760
      |                  LINES: 38->24|43->25|43->25|43->25|44->26|44->26|44->26|45->27|48->30|48->30|49->31|49->31|49->31|49->31|49->31|49->31|49->31|50->32|50->32|50->32|50->32|51->33|51->33|51->33|52->34|53->35|53->35|53->35|53->35|54->36|54->36|54->36|55->37|55->37|55->37|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|56->38|57->39|58->40|59->41|60->42
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/select.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object select extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML select.
      | *
      | * Example:
      | * {{{
      | * @select(
      | *   field = myForm("mySelect"),
      | *   options = Seq(
      | *     "Foo" -> "foo text",
      | *     "Bar" -> "bar text",
      | *     "Baz" -> "baz text"
      | *    ),
      | *   Symbol("_default") -> "Choose One",
      | *   Symbol("_disabled") -> Seq("FooKey", "BazKey")
      | *   Symbol("cust_att_name") -> "cust_att_value"
      | * )
      | * }}}
      | *
      | * @param field The form field.
      | * @param options Sequence of options as pairs of value and HTML.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*24.2*/(field: play.api.data.Field, options: Seq[(String,String)], args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*25.2*/input(field, args:_*)/*25.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*25.56*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*26.6*/defining( if( htmlArgs.contains(Symbol("multiple")) ) "%s[]".format(name) else name )/*26.91*/ { selectName =>_display_(Seq[Any](format.raw/*26.107*/(${"\"" * 3}
      |    ${"\"" * 3}),_display_(/*27.6*/defining( field.indexes.nonEmpty && htmlArgs.contains(Symbol("multiple")) match {
      |            case true => field.indexes.map( i => field("[%s]".format(i)).value ).flatten.toSet
      |            case _ => field.value.toSet
      |    })/*30.7*/{ selectedValues =>_display_(Seq[Any](format.raw/*30.26*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*31.9*/(${"\"" * 3}<select id=${"\"" * 3}"),_display_(/*31.22*/id),format.raw/*31.24*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*31.33*/selectName),format.raw/*31.43*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*31.46*/toHtmlArgs(htmlArgs)),format.raw/*31.66*/(${"\"" * 3}>
      |            ${"\"" * 3}),_display_(/*32.14*/args/*32.18*/.toMap.get(Symbol("_default")).map/*32.52*/ { defaultValue =>_display_(Seq[Any](format.raw/*32.70*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*33.17*/(${"\"" * 3}<option class="blank" value="">${"\"" * 3}),_display_(/*33.49*/translate(defaultValue)),format.raw/*33.72*/(${"\"" * 3}</option>
      |            ${"\"" * 3})))}),format.raw/*34.14*/(${"\"" * 3}
      |            ${"\"" * 3}),_display_(/*35.14*/options/*35.21*/.map/*35.25*/ { case (k, v) =>_display_(Seq[Any](format.raw/*35.42*/(${"\"" * 3}
      |                ${"\"" * 3}),_display_(/*36.18*/defining( selectedValues.contains(k) )/*36.56*/ { selected =>_display_(Seq[Any](format.raw/*36.70*/(${"\"" * 3}
      |                ${"\"" * 3}),_display_(/*37.18*/defining( args.toMap.get(Symbol("_disabled")).exists { case s: Seq[_] => s.asInstanceOf[Seq[String]].contains(k) })/*37.133*/{ disabled =>_display_(Seq[Any](format.raw/*37.146*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*38.17*/(${"\"" * 3}<option value=${"\"" * 3}"),_display_(/*38.33*/k),format.raw/*38.34*/(${"\"" * 3}${"\"" * 3}"),_display_(if(selected)/*38.48*/{_display_(Seq[Any](format.raw/*38.49*/(${"\"" * 3} ${"\"" * 3}),format.raw/*38.50*/(${"\"" * 3}selected="selected${"\"" * 3}")))} else {null} ),_display_(if(disabled)/*38.83*/{_display_(Seq[Any](format.raw/*38.84*/(${"\"" * 3} ${"\"" * 3}),format.raw/*38.85*/(${"\"" * 3}disabled${"\"" * 3})))} else {null} ),format.raw/*38.94*/(${"\"" * 3}>${"\"" * 3}),_display_(/*38.96*/v),format.raw/*38.97*/(${"\"" * 3}</option>
      |            ${"\"" * 3})))})))})))}),format.raw/*39.16*/(${"\"" * 3}
      |        ${"\"" * 3}),format.raw/*40.9*/(${"\"" * 3}</select>
      |    ${"\"" * 3})))})))}),format.raw/*41.7*/(${"\"" * 3}
      |${"\"" * 3})))}),format.raw/*42.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,options:Seq[(String,String)],args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,options,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Seq[(String,String)],Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,options,args) => (handler,messages) => apply(field,options,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/select.scala.html
      |                  HASH: cc0d6a16c55701852a1c335224961bd913bd0a80
      |                  MATRIX: 1261->552|1514->712|1544->733|1615->766|1647->772|1741->857|1796->873|1828->879|2059->1102|2116->1121|2152->1130|2192->1143|2215->1145|2251->1154|2282->1164|2312->1167|2353->1187|2395->1202|2408->1206|2451->1240|2507->1258|2552->1275|2611->1307|2655->1330|2709->1353|2750->1367|2766->1374|2779->1378|2834->1395|2879->1413|2926->1451|2978->1465|3023->1483|3148->1598|3200->1611|3245->1628|3288->1644|3310->1645|3351->1659|3390->1660|3419->1661|3495->1694|3534->1695|3563->1696|3616->1705|3645->1707|3667->1708|3729->1733|3765->1742|3814->1758|3846->1760
      |                  LINES: 37->24|42->25|42->25|42->25|43->26|43->26|43->26|44->27|47->30|47->30|48->31|48->31|48->31|48->31|48->31|48->31|48->31|49->32|49->32|49->32|49->32|50->33|50->33|50->33|51->34|52->35|52->35|52->35|52->35|53->36|53->36|53->36|54->37|54->37|54->37|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|55->38|56->39|57->40|58->41|59->42
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/style.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object style extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[Array[(Symbol,String)],Html,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      |* Generate an inline style with CSP nonce.
      |*
      |* Example:
      |* {{{
      |* @style(args = Symbol("type") -> "text/css") {
      |*   ...
      |* }
      |* }}}
      |*
      |* See <a href="https://www.w3.org/TR/html51/document-metadata.html#elementdef-style">style element</a>
      |* for more details.
      |*
      |* @param args Set of extra HTML attributes.
      |* @param body The style body.
      |*/
      |  def apply/*17.2*/(args: (Symbol,String)*)(body: => Html)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*18.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*19.1*/(${"\"" * 3}<style ${"\"" * 3}),_display_(/*19.9*/{CSPNonce.attr}),format.raw/*19.24*/(${"\"" * 3} ${"\"" * 3}),_display_(/*19.26*/toHtmlArgs(args.toMap)),format.raw/*19.48*/(${"\"" * 3}>${"\"" * 3}),_display_(/*19.50*/body),format.raw/*19.54*/(${"\"" * 3}</style>${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(args:Array[(Symbol,String)],body:Html,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(args.toIndexedSeq*)(body)(request)
      |
      |  def f:((Array[(Symbol,String)]) => (=> Html) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (args) => (body) => (request) => apply(args.toIndexedSeq*)(body)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/style.scala.html
      |                  HASH: adb03062cce963c9a2ac3e13ac1a5626a93ab349
      |                  MATRIX: 1029->337|1209->423|1237->424|1271->432|1307->447|1336->449|1379->471|1408->473|1433->477
      |                  LINES: 31->17|36->18|37->19|37->19|37->19|37->19|37->19|37->19|37->19
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/style.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object style extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[Array[(Symbol,String)],Html,play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      |* Generate an inline style with CSP nonce.
      |*
      |* Example:
      |* {{{
      |* @style(args = Symbol("type") -> "text/css") {
      |*   ...
      |* }
      |* }}}
      |*
      |* See <a href="https://www.w3.org/TR/html51/document-metadata.html#elementdef-style">style element</a>
      |* for more details.
      |*
      |* @param args Set of extra HTML attributes.
      |* @param body The style body.
      |*/
      |  def apply/*17.2*/(args: (Symbol,String)*)(body: => Html)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*18.1*/(${"\"" * 3}
      |${"\"" * 3}),format.raw/*19.1*/(${"\"" * 3}<style ${"\"" * 3}),_display_(/*19.9*/{CSPNonce.attr}),format.raw/*19.24*/(${"\"" * 3} ${"\"" * 3}),_display_(/*19.26*/toHtmlArgs(args.toMap)),format.raw/*19.48*/(${"\"" * 3}>${"\"" * 3}),_display_(/*19.50*/body),format.raw/*19.54*/(${"\"" * 3}</style>${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(args:Array[(Symbol,String)],body:Html,request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(args.toIndexedSeq: _*)(body)(request)
      |
      |  def f:((Array[(Symbol,String)]) => (=> Html) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (args) => (body) => (request) => apply(args.toIndexedSeq: _*)(body)(request)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/style.scala.html
      |                  HASH: 201ee57d526ee88230703138150c45fcc2ab6741
      |                  MATRIX: 991->337|1171->423|1199->424|1233->432|1269->447|1298->449|1341->471|1370->473|1395->477
      |                  LINES: 30->17|35->18|36->19|36->19|36->19|36->19|36->19|36->19|36->19
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/textarea.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object textarea extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML textarea.
      | *
      | * Example:
      | * {{{
      | * @textarea(field = myForm("address"), args = Symbol("rows") -> 3, Symbol("cols") -> 50)
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*14.2*/input(field, args:_*)/*14.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*14.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*15.5*/(${"\"" * 3}<textarea id=${"\"" * 3}"),_display_(/*15.20*/id),format.raw/*15.22*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*15.31*/name),format.raw/*15.35*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*15.38*/toHtmlArgs(htmlArgs)),format.raw/*15.58*/(${"\"" * 3}>${"\"" * 3}),_display_(/*15.60*/value),format.raw/*15.65*/(${"\"" * 3}</textarea>
      |${"\"" * 3})))}),format.raw/*16.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/textarea.scala.html
      |                  HASH: 59e0b9d010b641260ce1569bc91329f05489bf92
      |                  MATRIX: 1002->274|1224->403|1254->424|1325->457|1357->462|1399->477|1422->479|1458->488|1483->492|1513->495|1554->515|1583->517|1609->522|1652->535
      |                  LINES: 27->13|32->14|32->14|32->14|33->15|33->15|33->15|33->15|33->15|33->15|33->15|33->15|33->15|34->16
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/helper/textarea.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.helper
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object textarea extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[play.api.data.Field,Array[(Symbol,Any)],FieldConstructor,play.api.i18n.MessagesProvider,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**
      | * Generate an HTML textarea.
      | *
      | * Example:
      | * {{{
      | * @textarea(field = myForm("address"), args = Symbol("rows") -> 3, Symbol("cols") -> 50)
      | * }}}
      | *
      | * @param field The form field.
      | * @param args Set of extra attributes.
      | * @param handler The field constructor.
      | */
      |  def apply/*13.2*/(field: play.api.data.Field, args: (Symbol,Any)*)(implicit handler: FieldConstructor, messages: play.api.i18n.MessagesProvider):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](_display_(/*14.2*/input(field, args:_*)/*14.23*/ { (id, name, value, htmlArgs) =>_display_(Seq[Any](format.raw/*14.56*/(${"\"" * 3}
      |    ${"\"" * 3}),format.raw/*15.5*/(${"\"" * 3}<textarea id=${"\"" * 3}"),_display_(/*15.20*/id),format.raw/*15.22*/(${"\"" * 3}" name=${"\"" * 3}"),_display_(/*15.31*/name),format.raw/*15.35*/(${"\"" * 3}" ${"\"" * 3}),_display_(/*15.38*/toHtmlArgs(htmlArgs)),format.raw/*15.58*/(${"\"" * 3}>${"\"" * 3}),_display_(/*15.60*/value),format.raw/*15.65*/(${"\"" * 3}</textarea>
      |${"\"" * 3})))}),format.raw/*16.2*/(${"\"" * 3}
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(field:play.api.data.Field,args:Array[(Symbol,Any)],handler:FieldConstructor,messages:play.api.i18n.MessagesProvider): play.twirl.api.HtmlFormat.Appendable = apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def f:((play.api.data.Field,Array[(Symbol,Any)]) => (FieldConstructor,play.api.i18n.MessagesProvider) => play.twirl.api.HtmlFormat.Appendable) = (field,args) => (handler,messages) => apply(field,args.toIndexedSeq: _*)(handler,messages)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/helper/textarea.scala.html
      |                  HASH: 26326b31736e61a475599435ab28a885c47a7ee8
      |                  MATRIX: 964->274|1186->403|1216->424|1287->457|1319->462|1361->477|1384->479|1420->488|1445->492|1475->495|1516->515|1545->517|1571->522|1614->535
      |                  LINES: 26->13|31->14|31->14|31->14|32->15|32->15|32->15|32->15|32->15|32->15|32->15|32->15|32->15|33->16
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm3""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/play20/manual.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.play20
      |
      |import _root_.play.twirl.api.TwirlFeatureImports.*
      |import _root_.play.twirl.api.TwirlHelperImports.*
      |import scala.language.adhocExtensions
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object manual extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[String,Option[String],Option[String],String => String,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**/
      |  def apply/*1.2*/(title: String, main: Option[String], sidebar: Option[String], locate: String => String):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*2.1*/(${"\"" * 3}<html>
      |    <head>
      |        <title>${"\"" * 3}),_display_(/*4.17*/title),format.raw/*4.22*/(${"\"" * 3}</title>
      |        <link rel="stylesheet" media="screen" href="/@documentation/resources/style/main.css"></link>
      |        <script type="text/javascript" src='/@documentation/resources/${"\"" * 3}),_display_(/*6.73*/locate("jquery.min.js")),format.raw/*6.96*/(${"\"" * 3}'></script>
      |        <script type="text/javascript" src="/@documentation/resources/style/main.js"></script>
      |    </head>
      |    <body>
      |
      |        <section id="top">
      |            <div class="wrapper">
      |                <h1><a href="/@documentation">Manual, tutorials & references</a></h1>
      |                <nav>
      |                    <span class="versions">
      |                        <span>Browse APIs</span>
      |                        <select onchange="document.location=this.value">
      |                            <option selected disabled>Select language</option>
      |                            <option value="/@documentation/api/scala/index.html">Scala</option>
      |                            <option value="/@documentation/api/java/index.html">Java</option>
      |                        </select>
      |                    </span>
      |                </nav>
      |            </div>
      |        </section>
      |
      |        <div id="content" class="wrapper doc">
      |            <article id="main">
      |                ${"\"" * 3}),_display_(/*29.18*/main/*29.22*/.map/*29.26*/ { html =>_display_(Seq[Any](format.raw/*29.36*/(${"\"" * 3}
      |                    ${"\"" * 3}),_display_(/*30.22*/Html(html)),format.raw/*30.32*/(${"\"" * 3}
      |                ${"\"" * 3})))}/*31.18*/.getOrElse/*31.28*/ {_display_(Seq[Any](format.raw/*31.30*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*32.21*/(${"\"" * 3}<h1>Page not found [${"\"" * 3}),_display_(/*32.42*/title),format.raw/*32.47*/(${"\"" * 3}]</h1>
      |                ${"\"" * 3})))}),format.raw/*33.18*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*34.13*/(${"\"" * 3}</article>
      |            <aside>
      |                ${"\"" * 3}),_display_(/*36.18*/sidebar/*36.25*/.map(Html.apply)),format.raw/*36.41*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*37.13*/(${"\"" * 3}</aside>
      |        </div>
      |
      |        <style type="text/css">
      |            @import '/@documentation/resources/${"\"" * 3}),_display_(/*41.51*/locate("prettify.css")),format.raw/*41.73*/(${"\"" * 3}';
      |        </style>
      |        <script type="text/javascript" charset="utf-8" src='/@documentation/resources/${"\"" * 3}),_display_(/*43.89*/locate("prettify.js")),format.raw/*43.110*/(${"\"" * 3}'></script>
      |        <script type="text/javascript" charset="utf-8" src='/@documentation/resources/${"\"" * 3}),_display_(/*44.89*/locate("lang-scala.js")),format.raw/*44.112*/(${"\"" * 3}'></script>
      |        <script type="text/javascript">
      |            $$(function() ${"\"" * 3}),format.raw/*46.26*/(${"\"" * 3}{${"\"" * 3}),format.raw/*46.27*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*47.17*/(${"\"" * 3}window.prettyPrint && prettyPrint();
      |            ${"\"" * 3}),format.raw/*48.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*48.14*/(${"\"" * 3});
      |        </script>
      |
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(title:String,main:Option[String],sidebar:Option[String],locate:String => String): play.twirl.api.HtmlFormat.Appendable = apply(title,main,sidebar,locate)
      |
      |  def f:((String,Option[String],Option[String],String => String) => play.twirl.api.HtmlFormat.Appendable) = (title,main,sidebar,locate) => apply(title,main,sidebar,locate)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/play20/manual.scala.html
      |                  HASH: 796cb646f53391dc5d75ebffb73d22b4167d70c7
      |                  MATRIX: 697->1|879->90|939->124|964->129|1172->313|1215->336|2197->1295|2210->1299|2223->1303|2271->1313|2320->1335|2351->1345|2388->1363|2407->1373|2447->1375|2496->1396|2544->1417|2570->1422|2625->1446|2666->1459|2741->1507|2757->1514|2794->1530|2835->1543|2967->1650|3010->1672|3144->1780|3187->1801|3313->1901|3358->1924|3463->2001|3492->2002|3537->2019|3614->2068|3643->2069
      |                  LINES: 16->1|21->2|23->4|23->4|25->6|25->6|48->29|48->29|48->29|48->29|49->30|49->30|50->31|50->31|50->31|51->32|51->32|51->32|52->33|53->34|55->36|55->36|55->36|56->37|60->41|60->41|62->43|62->43|63->44|63->44|65->46|65->46|66->47|67->48|67->48
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|play@jvm213""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|views/html/play20/manual.template.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package views.html.play20
      |
      |import _root_.play.twirl.api.TwirlFeatureImports._
      |import _root_.play.twirl.api.TwirlHelperImports._
      |import _root_.play.twirl.api.Html
      |import _root_.play.twirl.api.JavaScript
      |import _root_.play.twirl.api.Txt
      |import _root_.play.twirl.api.Xml
      |import play.api.templates.PlayMagic._
      |
      |object manual extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template4[String,Option[String],Option[String],String => String,play.twirl.api.HtmlFormat.Appendable] {
      |
      |  /**/
      |  def apply/*1.2*/(title: String, main: Option[String], sidebar: Option[String], locate: String => String):play.twirl.api.HtmlFormat.Appendable = {
      |    _display_ {
      |      {
      |
      |
      |Seq[Any](format.raw/*2.1*/(${"\"" * 3}<html>
      |    <head>
      |        <title>${"\"" * 3}),_display_(/*4.17*/title),format.raw/*4.22*/(${"\"" * 3}</title>
      |        <link rel="stylesheet" media="screen" href="/@documentation/resources/style/main.css"></link>
      |        <script type="text/javascript" src='/@documentation/resources/${"\"" * 3}),_display_(/*6.73*/locate("jquery.min.js")),format.raw/*6.96*/(${"\"" * 3}'></script>
      |        <script type="text/javascript" src="/@documentation/resources/style/main.js"></script>
      |    </head>
      |    <body>
      |
      |        <section id="top">
      |            <div class="wrapper">
      |                <h1><a href="/@documentation">Manual, tutorials & references</a></h1>
      |                <nav>
      |                    <span class="versions">
      |                        <span>Browse APIs</span>
      |                        <select onchange="document.location=this.value">
      |                            <option selected disabled>Select language</option>
      |                            <option value="/@documentation/api/scala/index.html">Scala</option>
      |                            <option value="/@documentation/api/java/index.html">Java</option>
      |                        </select>
      |                    </span>
      |                </nav>
      |            </div>
      |        </section>
      |
      |        <div id="content" class="wrapper doc">
      |            <article id="main">
      |                ${"\"" * 3}),_display_(/*29.18*/main/*29.22*/.map/*29.26*/ { html =>_display_(Seq[Any](format.raw/*29.36*/(${"\"" * 3}
      |                    ${"\"" * 3}),_display_(/*30.22*/Html(html)),format.raw/*30.32*/(${"\"" * 3}
      |                ${"\"" * 3})))}/*31.18*/.getOrElse/*31.28*/ {_display_(Seq[Any](format.raw/*31.30*/(${"\"" * 3}
      |                    ${"\"" * 3}),format.raw/*32.21*/(${"\"" * 3}<h1>Page not found [${"\"" * 3}),_display_(/*32.42*/title),format.raw/*32.47*/(${"\"" * 3}]</h1>
      |                ${"\"" * 3})))}),format.raw/*33.18*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*34.13*/(${"\"" * 3}</article>
      |            <aside>
      |                ${"\"" * 3}),_display_(/*36.18*/sidebar/*36.25*/.map(Html.apply)),format.raw/*36.41*/(${"\"" * 3}
      |            ${"\"" * 3}),format.raw/*37.13*/(${"\"" * 3}</aside>
      |        </div>
      |
      |        <style type="text/css">
      |            @import '/@documentation/resources/${"\"" * 3}),_display_(/*41.51*/locate("prettify.css")),format.raw/*41.73*/(${"\"" * 3}';
      |        </style>
      |        <script type="text/javascript" charset="utf-8" src='/@documentation/resources/${"\"" * 3}),_display_(/*43.89*/locate("prettify.js")),format.raw/*43.110*/(${"\"" * 3}'></script>
      |        <script type="text/javascript" charset="utf-8" src='/@documentation/resources/${"\"" * 3}),_display_(/*44.89*/locate("lang-scala.js")),format.raw/*44.112*/(${"\"" * 3}'></script>
      |        <script type="text/javascript">
      |            $$(function() ${"\"" * 3}),format.raw/*46.26*/(${"\"" * 3}{${"\"" * 3}),format.raw/*46.27*/(${"\"" * 3}
      |                ${"\"" * 3}),format.raw/*47.17*/(${"\"" * 3}window.prettyPrint && prettyPrint();
      |            ${"\"" * 3}),format.raw/*48.13*/(${"\"" * 3}}${"\"" * 3}),format.raw/*48.14*/(${"\"" * 3});
      |        </script>
      |
      |    </body>
      |</html>
      |${"\"" * 3}))
      |      }
      |    }
      |  }
      |
      |  def render(title:String,main:Option[String],sidebar:Option[String],locate:String => String): play.twirl.api.HtmlFormat.Appendable = apply(title,main,sidebar,locate)
      |
      |  def f:((String,Option[String],Option[String],String => String) => play.twirl.api.HtmlFormat.Appendable) = (title,main,sidebar,locate) => apply(title,main,sidebar,locate)
      |
      |  def ref: this.type = this
      |
      |}
      |
      |
      |              /*
      |                  -- GENERATED --
      |                  SOURCE: core/play/src/main/scala/views/play20/manual.scala.html
      |                  HASH: e4641dc1709211620be0e479561432317149dbaa
      |                  MATRIX: 659->1|841->90|901->124|926->129|1134->313|1177->336|2159->1295|2172->1299|2185->1303|2233->1313|2282->1335|2313->1345|2350->1363|2369->1373|2409->1375|2458->1396|2506->1417|2532->1422|2587->1446|2628->1459|2703->1507|2719->1514|2756->1530|2797->1543|2929->1650|2972->1672|3106->1780|3149->1801|3275->1901|3320->1924|3425->2001|3454->2002|3499->2019|3576->2068|3605->2069
      |                  LINES: 15->1|20->2|22->4|22->4|24->6|24->6|47->29|47->29|47->29|47->29|48->30|48->30|49->31|49->31|49->31|50->32|50->32|50->32|51->33|52->34|54->36|54->36|54->36|55->37|59->41|59->41|61->43|61->43|62->44|62->44|64->46|64->46|65->47|66->48|66->48
      |                  -- GENERATED --
      |              */
      |          """.stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}