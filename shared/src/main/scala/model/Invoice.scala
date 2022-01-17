package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import shared.utils.Routines._

class Invoice(
  var recipient:   Contact,
  var address:     Address,
  var number:      String,
  var date:        Int,         // format yyyymmdd
  val currency:    String,
  var total:       Double,
  var vat:         Double,
  var items:       Seq[InvoiceItem]
) {

  /** toTx - convert to transfer format
   * 
   */
  def toTx(): InvoiceTx = {
    InvoiceTx(recipient.toString, address.toString, date, currency, total, vat, items.map(_.toString))
  }

  def getTotal   = "%1.2f".format(total)
  def getTax     = "%1.2f".format(total / (100+vat) * vat)
  def getDate(lang: String) = int2date(date, lang)
}

case class InvoiceItem(
   val description: String,
   val price:       Double,
   val currency:    String,
) {
  override def toString() = s"${description}·${price}·${currency} "
  def getPrice = "%1.2f".format(price)
}

case class InvoiceTx(
  recipient : String,
  address   : String,
  date      : Int,
  currency  : String,
  total     : Double,
  vat       : Double,
  items     : Seq[String]
)

object InvoiceTx {
  implicit def rw: RW[InvoiceTx] = macroRW
}
