package scalajs.usecase.dialog

import scala.scalajs._
import org.querki.jquery._           
import shared.utils.UseCaseParam
import scalajs.usecase.component._


object DlgSpinner extends BasicHtml
{

  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgSpinner", "dlg.spinner", "DlgSpinner", "dlgspinner", scalajs.AppEnv.getMessage _ ) 
 
  /** Handles spinner dialog
   *
   *  start with msg
   *  result with msg
   *  error with msg
   *  close with no message
   */  
  def start(msg: String) = {
    println("DlgSpinner.start")
    loadModal(clientviews.dialog.html.DlgSpinner(), ucp)
    $("#APP__DlgSpinner__Modal").modal("show")
    removeClass(gUE("Type"), "border-danger", "border-success", "border-5")
    addClass(gUE("Type"), "border-secondary")
    setHtml(gUE("Content"), msg)
    
  }  

  def error(msg: String) = {
    setHtml(gUE("Content"), msg)
    removeClass(gUE("Type"), "border-secondary")
    addClass(gUE("Type"), "border-danger", "border-5")
    setHtml(gUE("Image"), """<img class="img-fluid" src="assets/img/pullhair150.gif" alt="Error ...">""")
  }  

  def result(msg: String) = {
    setHtml(gUE("Content"), msg)
    removeClass(gUE("Type"), "border-secondary")
    addClass(gUE("Type"), "border-success", "border-5")
    setHtml(gUE("Image"), """<img class="img-fluid" src="assets/img/highfive150.gif" alt="Error ...">""")
  }    

  def close() = {
    $("#APP__DlgSpinner__Modal").modal("hide")
    removeClass(gUE("Type"), "border-secondary")
    $("#APP__DlgSpinner__Modal").modal("hide")
  }  
}