package gnieh.diffson
package jsonpatch
package test

import cats.implicits._

import scala.util.Try

import org.scalatest._

abstract class TestJsonPatch[JsValue](implicit val J: JsonProvider[JsValue]) extends FlatSpec
  with Matchers {

  import J._

  def jsonInt(i: Int): JsValue
  def parseJson(s: String): JsValue

  def parsePointer(s: String): Pointer =
    Pointer.parse[Try](s).get

  "applying an 'add' operation" should "add the field to the object if it does not exist" in {
    val op = Add[JsValue](parsePointer("/lbl"), jsonInt(17))
    op(parseJson("{}")) should be(parseJson("{ \"lbl\": 17 } "))
  }

  "applying an 'add' operation to /foo/" should "add a value with an empty string as the key" in {
    val op = Add[JsValue](parsePointer("/foo/"), jsonInt(17))
    op(parseJson("{ \"foo\": {} }")) should be(parseJson("{ \"foo\": {\"\": 17 } }"))
  }

  it should "replace the value if the pointer is the root" in {
    val op = Add[JsValue](parsePointer(""), jsonInt(17))
    op(parseJson("[1, 2, 3, 4]")) should be(jsonInt(17))
  }

  it should "replace the field value if it does exist" in {
    val op = Add[JsValue](parsePointer("/lbl"), jsonInt(17))
    op(parseJson("{ \"lbl\": true }")) should be(parseJson("{ \"lbl\": 17 } "))
  }

  it should "add an element to the array at the given index" in {
    val op1 = Add[JsValue](parsePointer("/1"), jsonInt(17))
    op1(parseJson("[1, 2, 3]")) should be(parseJson("[1, 17, 2, 3]"))
    val op2 = Add[JsValue](parsePointer("/0"), jsonInt(17))
    op2(parseJson("[1, 2, 3]")) should be(parseJson("[17, 1, 2, 3]"))
  }

  it should "add an element at the end of the array if the last element is '-'" in {
    val op = Add[JsValue](parsePointer("/-"), jsonInt(17))
    op(parseJson("[1, 2, 3]")) should be(parseJson("[1, 2, 3, 17]"))
  }

  it should "create a nested field if needed" in {
    val op = Add[JsValue](parsePointer("/lbl/lbl"), jsonInt(17))
    op(parseJson("{ \"lbl\": {} }")) should be(parseJson("{ \"lbl\": { \"lbl\": 17 } }"))
  }

  it should "throw an error if some element is missing in the middle of the path" in {
    a[PatchException] should be thrownBy {
      val op = Add[JsValue](parsePointer("/lbl/lbl"), jsonInt(17))
      op(parseJson("{}"))
    }
  }

  it should "throw an error if adding an element out of the array boundaries" in {
    a[PatchException] should be thrownBy {
      val op = Add[JsValue](parsePointer("/178"), jsonInt(17))
      op(parseJson("[1, 2]"))
    }
  }

  "removing a label of an object" should "result in the object being amputed from this label" in {
    val op = Remove[JsValue](parsePointer("/lbl"))
    op(parseJson("{ \"lbl\": 17, \"toto\": true }")) should be(parseJson("{ \"toto\": true }"))
  }

  "removing an element of an array" should "result in the array being amputed from this element" in {
    val op = Remove[JsValue](parsePointer("/2"))
    op(parseJson("[1, 2, 3, 4, 5]")) should be(parseJson("[1, 2, 4, 5]"))
  }

  "removing the '-' element of an array" should "result in an error being thrown" in {
    a[PatchException] should be thrownBy {
      val op = Remove[JsValue](parsePointer("/-"))
      op(parseJson("[1, 2, 3, 4]"))
    }
  }

  "removing an element out of the array boundaries" should "result in an error being thrown" in {
    a[PatchException] should be thrownBy {
      val op = Remove[JsValue](parsePointer("/20"))
      op(parseJson("[1, 2, 3, 4]"))
    }
  }

  "removing an unknown label from an object" should "result in an error being thrown" in {
    a[PatchException] should be thrownBy {
      val op = Remove[JsValue](parsePointer("/toto"))
      op(parseJson("{}"))
    }
  }

  "removing the root" should "result in an error being thrown" in {
    a[PatchException] should be thrownBy {
      val op = Remove[JsValue](parsePointer("/"))
      op(parseJson("{}"))
    }
  }

  "replacing an element in an object" should "result in this element being replaced" in {
    val op = Replace[JsValue](parsePointer("/lbl/lbl"), jsonInt(17))
    op(parseJson("""{"lbl": {"lbl": true, "gruik": 1}, "toto": 3}""")) should be(parseJson("""{"lbl": {"lbl": 17, "gruik": 1}, "toto": 3}"""))
  }

  "replacing an element in an array" should "result in this element being replaced" in {
    val op = Replace[JsValue](parsePointer("/3"), jsonInt(17))
    op(parseJson("[true, false, true, true, true]")) should be(parseJson("[true, false, true, 17,true]"))
  }

  "replacing the root" should "result in the value being completely replaced" in {
    val op = Replace[JsValue](parsePointer(""), jsonInt(17))
    op(parseJson("[1, 2, 3]")) should be(jsonInt(17))
  }

  "replacing a non existing element in an object" should "result in an error being thrown" in {
    a[PatchException] should be thrownBy {
      val op = Replace[JsValue](parsePointer("/1/lbl"), jsonInt(17))
      op(parseJson("[1, {}, true]"))
    }
  }

  "replacing the '-' element of an array" should "result in an error being thrown" in {
    a[PatchException] should be thrownBy {
      val op = Replace[JsValue](parsePointer("/-"), jsonInt(17))
      op(parseJson("[1, 2, 3, 4]"))
    }
  }

  "replacing an element out of the array boundaries" should "result in an error being thrown" in {
    a[PatchException] should be thrownBy {
      val op = Replace[JsValue](parsePointer("/20"), jsonInt(17))
      op(parseJson("[1, 2, 3, 4]"))
    }

    a[PatchException] should be thrownBy {
      val op = Replace[JsValue](parsePointer("/array/3/sub1"), jsonInt(17))
      op(parseJson("{\"array\":[\"bar1\",\"bar2\",{\"sub1\":\"bar3\"}]}"))
    }
  }

  "moving a value from an object to an array" should "result in the value being added to the array and removed from the object" in {
    val op = Move[JsValue](parsePointer("/0/lbl"), parsePointer("/1/1"))
    op(parseJson("[{ \"lbl\": 17, \"toto\": true }, [1, 2], \"plop\"]")) should be(
      parseJson("[{ \"toto\": true }, [1, 17, 2], \"plop\"]"))
  }

  "moving a value in a sub element" should "result in an error being thrown" in {
    a[PatchException] should be thrownBy {
      val op = Move[JsValue](parsePointer("/0"), parsePointer("/0/toto"))
      op(parseJson("0"))
    }
  }

  "moving the root" should "result in an error being thrown" in {
    a[PatchException] should be thrownBy {
      val op = Move[JsValue](parsePointer("/"), parsePointer("/toto"))
      op(parseJson("0"))
    }
  }
}
