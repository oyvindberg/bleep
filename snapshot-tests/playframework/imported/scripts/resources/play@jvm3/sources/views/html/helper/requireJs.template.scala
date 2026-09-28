
package views.html.helper

import _root_.play.twirl.api.TwirlFeatureImports.*
import _root_.play.twirl.api.TwirlHelperImports.*
import scala.language.adhocExtensions
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
                  HASH: f1f788871d0682d1402e6e85b07fd768d93bc2b4
                  MATRIX: 1205->529|1420->650|1490->693|1589->770|1625->778|1651->782
                  LINES: 29->16|34->17|34->17|34->17|34->17|34->17
                  -- GENERATED --
              */
          