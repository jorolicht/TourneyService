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
    removeClass(gE("Type", ucp), "border-danger", "border-success", "border-5")
    addClass(gE("Type", ucp), "border-secondary")
    setHtml("Content", msg)
    
  }  

  def error(msg: String) = {
    setHtml("Content", msg)
    removeClass(gE("Type", ucp), "border-secondary")
    addClass(gE("Type", ucp), "border-danger", "border-5")
    setHtml("Image", """<img class="img-fluid" src="assets/img/pullhair150.gif" alt="Error ...">""")
  }  

  def result(msg: String) = {
    setHtml("Content", msg)
    removeClass(gE("Type", ucp), "border-secondary")
    addClass(gE("Type", ucp), "border-success", "border-5")
    setHtml("Image", """<img class="img-fluid" src="assets/img/highfive150.gif" alt="Error ...">""")
  }    

  def close() = {
    $("#APP__DlgSpinner__Modal").modal("hide")
    println("Close Modal")
    removeClass(gE("Type", ucp), "border-secondary")
    $("#APP__DlgSpinner__Modal").modal("hide")
  }  
}