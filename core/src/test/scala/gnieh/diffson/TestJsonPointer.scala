package gnieh.diffson
package jsonpatch
package test

import scala.util.Try

import cats.implicits._

import org.scalatest._

abstract class TestJsonPointer[JsValue](implicit val J: JsonProvider[JsValue]) extends FlatSpec with Matchers {

  import J._

  def jsonInt(i: Int): JsValue
  def jsonBoolean(b: Boolean): JsValue
  def parseJson(s: String): JsValue

  def parsePointer(s: String): Pointer =
    Pointer.parse[Try](s).get

  "an empty string" should "be parsed as an empty pointer" in {
    parsePointer("").path should be(Pointer.Root)
  }

  "the root pointer" should "be parsed as the pointer to empty element at root" in {
    parsePointer("/").path should be(Pointer(""))
  }

  "a string with a trailing forward slash" should "parse with an empty final element" in {
    parsePointer("/foo/").path should be(Pointer("foo", ""))
  }

  "a pointer string with one chunk" should "be parsed as a pointer with one element" in {
    parsePointer("/test").path should be(Pointer("test"))
  }

  "occurrences of ~0" should "be replaced by occurrences of ~" in {
    parsePointer("/~0/test/~0~0plop").path should be(Pointer("~", "test", "~~plop"))
  }

  "occurrences of ~1" should "be replaced by occurrences of /" in {
    parsePointer("/test~1/~1/plop").path should be(Pointer("test/", "/", "plop"))
  }

  "occurrences of ~" should "be directly followed by either 0 or 1" in {
    a[PointerException] should be thrownBy { parsePointer("/~") }
    a[PointerException] should be thrownBy { parsePointer("/~3") }
    a[PointerException] should be thrownBy { parsePointer("/~d") }
  }

  "a non empty pointer" should "start with a /" in {
    a[PointerException] should be thrownBy { parsePointer("test") }
  }

  "a pointer to a label" should "be evaluated to the label value if it is one level deep" in {
    parsePointer("/label").evaluate(parseJson("{\"label\": true}")) should be(jsonBoolean(true))
  }

  it should "be evaluated to the end label value if it is several levels deep" in {
    parsePointer("/l1/l2/l3").evaluate(parseJson("""{"l1": {"l2": { "l3": 17 } } }""")) should be(jsonInt(17))
  }

  it should "be evaluated to nothing if the final element is unknown" in {
    parsePointer("/lbl").evaluate(parseJson("{}")) should be(JsNull)
  }

  it should "produce an error if there is an unknown element in the middle of the pointer" in {
    a[PointerException] should be thrownBy { parsePointer("/lbl/test").evaluate(parseJson("{}")) }
  }

  "a pointer to an array element" should "be evaluated to the value at the given index" in {
    parsePointer("/1").evaluate(parseJson("[1, 2, 3]")) should be(jsonInt(2))
    parsePointer("/lbl/4").evaluate(parseJson("{ \"lbl\": [3, 7, 5, 4, 7] }")) should be(jsonInt(7))
  }

  it should "produce an error if it is out of the array bounds" in {
    a[PointerException] should be thrownBy { parsePointer("/4").evaluate(parseJson("[1]")) }
  }

  it should "produce an error if it is the '-' element" in {
    a[PointerException] should be thrownBy { parsePointer("/-").evaluate(parseJson("[1]")) }
  }

}
