package test.utest.examples

import utest._


object HelloTests extends TestSuite {
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


  }
}