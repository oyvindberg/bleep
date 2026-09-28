
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object javascriptRouter extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template3[String,Array[play.api.routing.JavaScriptReverseRoute],play.api.mvc.RequestHeader,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * Generates a Javascript object that lets you refer 
 * to your application's routes in Javascript code
 *
 * Example:
 * {{{
 * @javascriptRouter("jsRoutes")(
 *   routes.javascript.Users.list,
 *   routes.javascript.Application.index
 * )
 * }}}
 *
 * You can access your routes in JavaScript without hardcoded URL's, e.g. assuming jQuery's ajax function:
 * {{{
 * $.ajax(jsRoutes.controllers.Users.list()).done( /* */ ).fail( /* */ )
 * }}}
 * Each action in the generated object also has the following properties:
 * * *type*: HTTP method
 * * *url*: the url to be used
 * 
 * @param name The javascript object name.
 * @param routes Set of routes to include in this javascript router.
 */
  def apply/*24.2*/(name:String = "Router")(routes: play.api.routing.JavaScriptReverseRoute*)(implicit request: play.api.mvc.RequestHeader):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](_display_(/*25.2*/script(Symbol("type") -> "text/javascript")/*25.45*/ {_display_(Seq[Any](format.raw/*25.47*/("""
    """),_display_(/*26.6*/Html(play.api.routing.JavaScriptReverseRouter(name)(routes: _*).body.replace("/", "\\/"))),format.raw/*26.95*/("""
""")))}))
      }
    }
  }

  def render(name:String,routes:Array[play.api.routing.JavaScriptReverseRoute],request:play.api.mvc.RequestHeader): play.twirl.api.HtmlFormat.Appendable = apply(name)(routes.toIndexedSeq: _*)(request)

  def f:((String) => (Array[play.api.routing.JavaScriptReverseRoute]) => (play.api.mvc.RequestHeader) => play.twirl.api.HtmlFormat.Appendable) = (name) => (routes) => (request) => apply(name)(routes.toIndexedSeq: _*)(request)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/javascriptRouter.scala.html
                  HASH: 13f437cea1e3ade0e8935851b91ebec26647ac84
                  MATRIX: 1392->701|1607->823|1659->866|1699->868|1731->874|1841->963
                  LINES: 37->24|42->25|42->25|42->25|43->26|43->26
                  -- GENERATED --
              */
          