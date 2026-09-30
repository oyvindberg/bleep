
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports._
import _root_.play.twirl.api.TwirlHelperImports._
import _root_.play.twirl.api.Html
import _root_.play.twirl.api.JavaScript
import _root_.play.twirl.api.Txt
import _root_.play.twirl.api.Xml
import play.api.templates.PlayMagic._

object requireJs extends _root_.play.twirl.api.BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,_root_.play.twirl.api.Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with _root_.play.twirl.api.Template5[String,String,Boolean,String,String,play.twirl.api.HtmlFormat.Appendable] {

  /**
 * RequireJS Javascript module loader.
 *
 * Example:
 * {{{
 * @requireJs(core = routes.Assets.at("javascripts/require.js").url, module = routes.Assets.at("javascripts/main").url, isProd = true)
 * }}}
 *
 * @param module Javascript module in question.
 * @param core Reference to require.js.
 * @param isProd true if the javascript should be minified, false otherwise.
 * @param productionFolderPrefix Prefix of Javascript production folder, default "-min".
 * @param folder Javascript folder, default "javascripts".
 */
  def apply/*16.2*/(module: String, core: String, isProd: Boolean, productionFolderPrefix: String = "-min", folder: String = "javascripts"):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {


Seq[Any](format.raw/*17.1*/("""<script type="text/javascript" data-main=""""),_display_(/*17.44*/{if(isProd) module.replace(folder,folder+productionFolderPrefix) else module}),format.raw/*17.121*/("""" src=""""),_display_(/*17.129*/core),format.raw/*17.133*/(""""></script>
"""))
      }
    }
  }

  def render(module:String,core:String,isProd:Boolean,productionFolderPrefix:String,folder:String): play.twirl.api.HtmlFormat.Appendable = apply(module,core,isProd,productionFolderPrefix,folder)

  def f:((String,String,Boolean,String,String) => play.twirl.api.HtmlFormat.Appendable) = (module,core,isProd,productionFolderPrefix,folder) => apply(module,core,isProd,productionFolderPrefix,folder)

  def ref: this.type = this

}


              /*
                  -- GENERATED --
                  SOURCE: core/play/src/main/scala/views/helper/requireJs.scala.html
                  HASH: 9c592686bc2b1e73611309bba7097889155b3907
                  MATRIX: 1167->529|1382->650|1452->693|1551->770|1587->778|1613->782
                  LINES: 28->16|33->17|33->17|33->17|33->17|33->17
                  -- GENERATED --
              */
          