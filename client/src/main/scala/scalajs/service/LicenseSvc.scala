package scalajs.service

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{ Either, Success, Failure}

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.concurrent.duration._

import scala.scalajs._
import upickle.default._

import shared.utils._
import shared.model._
import scalajs._

trait LicenseSvc extends WrapperSvc {

  /** base64enc
    * 
    * @param input
    * @return
    */
  def base64enc(input: String): String = {
    import org.encoding.Base64._
    input.getBytes.toBase64
  }

  /** getOwnLicense - get (own) license
   * 
   */ 
  def getOwnLicense():  Future[Either[Error, License]] = 
    getJson("/license/getOwn","").map {
      case Left(err)  => Left(err)
      case Right(res) => License.decode(res) 
    }


  /** getAllLicense - for administrator returns all available licenses
   * 
   */ 
  def getAllLicense(): Future[Seq[License]] = 
    getJson("/license/getAll","").map {
      case Left(err)  => Seq()
      case Right(res) => {
        val lics = read[Licenses](res)
        (for { lic <- lics.list } yield { License.obify(lic) }).toSeq
      } 
    }

  /** requestLicense request a license for a director
   *
   * @param request license request
   * @return either an error or the Tripel (generated license, orgId, orgDir)
   */
  def requestLicense(request: LicRequest): Future[Either[Error, (String,Long,String)]] = {
    postJson("/license/request", "", request.stringify).map {
      case Left(err)  => Left(err)
      case Right(res) => {
        try Right(read[(String,Long,String)](res))
        catch { case _:Throwable => Left(Error("err0123.svc.requestLicense", res)) }
      }
    }
  }
    
  // check if license is already in use
  def hasLicense(club: String): Future[Either[Error, Boolean]] = 
    getJson("/license/has", s"club=${club}").map {
      case Left(err)  => Left(err)
      case Right(res) => Return.decode2Boolean(res)
    }

  // check if clubname is available or already in use
  def licenseAvailable(club: String): Future[Either[Error, Boolean]] = 
    getJson("/license/available", s"club=${club}").map {
      case Left(err)  => Left(err)
      case Right(res) => Return.decode2Boolean(res)
    }  


  // delete a license
  def deleteLicense(liId: Long): Future[Either[Error, Int]] = 
    postJson("/license/delete", s"liId=${liId.toString}").map {
      case Left(err)  => Left(err)
      case Right(res) => Return.decode2Int(res)
    }


  // update a license only name, email and allow/deny updates
  def updateLicense(license: License): Future[Either[Error, Int]]  = {
    postJson("/license/update", "", license.toString).map {
      case Left(err)  => Left(err)
      case Right(res) => Return.decode2Int(res)
    }
  } 

  /** sendCode2Email - send user the verification code
   * 
   * @param code 4 digit verification code
   * @param email address of the user
   * @param name of user 
   */
  def sendLicenseVerifyCode(code: Int, email: String, name: String): Future[Either[Error, Boolean]] = 
    getJson("/license/sendVerifyCode", s"code=${code}&email=${email}&name=${name}").map {
      case Left(err)  => Left(err)
      case Right(res) => Return.decode2Boolean(res) 
    }

  /** sendLicense license send to club (registered main user)
   * 
   * @param orgId
   * @param invoice 
   */  
  def sendLicense(orgId: Long, invoice: String=""): Future[Either[Error, Boolean]] = 
    postJson("/license/sendLicense", s"orgId=${orgId}&invoice=${invoice}").map {
      case Left(err)  => Left(err)
      case Right(res) => Return.decode2Boolean(res)
    }

  /** genInvoice - generates invoice for a full license
    * 
    * @param contact
    * @param address
    * @param orgDir
    * @param orgId
    * @return name of generated invoice (pdf file)
    * 
    */
  def genInvoice(contact: Contact, address: Address, orgDir: String, orgId: Long): Future[Either[Error, String]] = {
    postJson("/license/invoice", s"orgDir=${orgDir}&contact=${contact.encode}&address=${address.encode}&orgId=${orgId}").map {
      case Left(err)  => Left(err)
      case Right(res) => Return.decode2String(res)
    }
  }

}  