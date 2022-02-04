package scalajs.service

import scala.concurrent.duration._
import scala.concurrent._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global


import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.NodeListOf
import org.scalajs.dom.raw.DOMList
import org.scalajs.dom.raw.Node
import org.scalajs.dom.document

import shared.model._
import shared.utils._
import shared.utils.Constants._


import scalajs.{ App, AppEnv }
import scalajs.usecase.Helper
import scalajs.usecase.dialog.{ DlgBox }


trait UIComponentServices {

  implicit class NodeListSeq[T <: Node](nodes: DOMList[T]) extends IndexedSeq[T] {
    override def foreach[U](f: T => U): Unit = {
      for (i <- 0 until nodes.length) {
        f(nodes(i))
      }
    }
    override def length: Int = nodes.length
    override def apply(idx: Int): T = nodes(idx)
  }

  def errLog(text: String): Unit = dom.console.log(s"%c ERROR: ${text}", "color: #B22222")

  def innerText(id: String, content: String): Unit = {
    try document.getElementById(id).asInstanceOf[HTMLElement].innerText = content
    catch { case _: Throwable => errLog(s"innerText => id: ${id} content: ${content.take(10)}") } 
  }


  // setTableRow
  def selTableRow(id: String, classText: String="text-white", classBg: String="bg-secondary") = {
    val elems2rem = document.getElementById(id).parentNode.asInstanceOf[HTMLElement].getElementsByTagName("tr").asInstanceOf[NodeListOf[HTMLElement]]
    val selElem   = document.getElementById(id)
    elems2rem.map { elem => elem.classList.remove(classText); elem.classList.remove(classBg) }  
    selElem.classList.add(classText)
    selElem.classList.add(classBg)
  }  


 // confirm dialog
  def confirmDlg(title: String, msg: String): Future[Boolean] =
    DlgBox.showStd(title, msg, Seq("cancel", "ok")).map { _ match {
     case 2 => true
     case _ => false
    }}


  // showAlert  
  def showAlert(text: String): String = {
    s"""
      |<div class="alert alert-info" role="alert">
      |  <span class="tuse-font-1">${text}</span>
      |</div>
    """.stripMargin('|')
  }



}