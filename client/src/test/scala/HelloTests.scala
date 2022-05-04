package test.utest.examples

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import utest._
import scalajs.usecase.home
import scalajs.service._

object HelloTests extends TestSuite
  with TourneySvc 
{
  val tests = Tests{
    test("test1"){
      throw new Exception("test1")
    }
    test("test2"){
      1
    }
    test("test3"){
      val a = List[Byte](1, 2)
      a(10)
    }
    test("test4"){
       val x = "Robert"
       assert(x.startsWith("H"))
     }

    test("test4"){
      // import org.scalajs.dom._
      // val xhr = new XMLHttpRequest()

      // xhr.open("GET", "http://127.0.0.1:9000", true)
      // xhr.onload = { (e: Event) =>
      //   if (xhr.status == 200) {
      //     println("onload 200")
      //   }
      // }
      // xhr.send()


      ping(100L, "Hallo").map {
        case Left(err)  => assert(err.msgCode == "")
        case Right(res) => assert(res != "") 
      }
    }  

  }
}

object ConsoleTest {
  def hallo() = println("xxxx")
}


// val xhr = new XMLHttpRequest()

// xhr.open("GET",
//   "https://api.twitter.com/1.1/search/" +
//   "tweets.json?q=%23scalajs"
// )
// xhr.onload = { (e: Event) =>
//   if (xhr.status == 200) {
//     val r = JSON.parse(xhr.responseText)
//     $("#tweets").html(parseTweets(r))
//   }
// }
// xhr.send()