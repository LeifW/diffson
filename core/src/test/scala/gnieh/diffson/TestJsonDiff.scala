package gnieh.diffson
package jsonpatch
package test

import cats.implicits._

import scala.util.Try

import org.scalatest._

import scala.language.higherKinds

abstract class TestJsonDiff[JsValue](implicit val J: JsonProvider[JsValue]) extends FlatSpec with Matchers {

  import J._

  def jsonInt(i: Int): JsValue
  def jsonBoolean(b: Boolean): JsValue
  def jsonString(s: String): JsValue
  def parseJson(s: String): JsValue
  def parseJsonPatch(s: String): JsonPatch[JsValue]

  val dp = JsonDiff.provider[JsValue]()
  val dpo = JsonDiff.provider[JsValue](remember = true)

  def diff(json1: JsValue, json2: JsValue): JsonPatch[JsValue] =
    dp.diff(json1, json2)

  def diffOld(json1: JsValue, json2: JsValue): JsonPatch[JsValue] =
    dpo.diff(json1, json2)

  def patch(json: JsValue, p: JsonPatch[JsValue]): JsValue =
    p[Try](json).get

  "a diff" should "be empty if created between two equal values" in {
    val json = parseJson("true")
    diff(json, json) should be(JsonPatch(Nil))
  }

  it should "be a simple replacement if the two values are completely different" in {
    diff(parseJson("true"), parseJson("13")) should be(JsonPatch(Replace(Pointer.Root, jsonInt(13))))
  }

  it should "contain an add operation for each added field" in {
    val json1 = parseJson("""{"lbl": 32}""")
    val json2 = parseJson("""{"lbl": 32, "new": false}""")
    val json3 = parseJson("""{"lbl": 32, "new1": false, "new2": null}""")
    val json4 = parseJson("""{"a": 3, "b": {"a": true }}""")
    val json5 = parseJson("""{"a": 3, "b": {"a": true, "b": 43}, "c": null}""")
    diff(json1, json2) should be(JsonPatch(Add[JsValue](Pointer("new"), jsonBoolean(false))))
    diff(json1, json3) should be(JsonPatch(Add[JsValue](Pointer("new2"), JsNull), Add[JsValue](Pointer("new1"), jsonBoolean(false))))
    diff(json4, json5) should be(JsonPatch(Add[JsValue](Pointer("b", "b"), jsonInt(43)), Add[JsValue](Pointer("c"), JsNull)))
  }

  it should "contain a remove operation for each removed field" in {
    val json1 = parseJson("""{"lbl": 32}""")
    val json2 = parseJson("""{"lbl": 32, "old": false}""")
    val json3 = parseJson("""{"lbl": 32, "old1": false, "old2": null}""")
    val json4 = parseJson("""{"a": 3, "b": {"a": true }}""")
    val json5 = parseJson("""{"a": 3, "b": {"a": true, "b": 43}, "c": null}""")
    diff(json2, json1) should be(JsonPatch(Remove[JsValue](Pointer("old"))))
    diff(json3, json1) should be(JsonPatch(Remove[JsValue](Pointer("old2")), Remove[JsValue](Pointer("old1"))))
    diff(json5, json4) should be(JsonPatch(Remove[JsValue](Pointer("b", "b")), Remove[JsValue](Pointer("c"))))
  }

  it should "correctly handle array diffs in objects" in {
    val json1 = parseJson("""{"lbl": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}""")
    val json2 = parseJson("""{"lbl": [1, 4, 5, 11, 6, 7]}""")
    diff(json1, json2) should be(JsonPatch(Remove[JsValue](Pointer("lbl", "2")), Remove[JsValue](Pointer("lbl", "1")), Add[JsValue](Pointer("lbl", "3"), jsonInt(11)), Remove[JsValue](Pointer("lbl", "8")), Remove[JsValue](Pointer("lbl", "7")), Remove[JsValue](Pointer("lbl", "6"))))
  }

  it should "contain a replace operation for each changed field value" in {
    val json1 = parseJson("""{"lbl": 32}""")
    val json2 = parseJson("""{"lbl": 60}""")
    val json3 = parseJson("""{"lbl": {"a": true}}""")
    val json4 = parseJson("""{"lbl": {"a": null}}""")
    diff(json1, json2) should be(JsonPatch(Replace(Pointer("lbl"), jsonInt(60))))
    diff(json1, json3) should be(JsonPatch(Replace(Pointer("lbl"), parseJson("""{"a": true}"""))))
    diff(json3, json4) should be(JsonPatch(Replace(Pointer("lbl", "a"), JsNull)))
  }

  it should "contain an add operation for each added element" in {
    val json1 = parseJson("[]")
    val json2 = parseJson("[1, 2, 3]")
    val json3 = parseJson("[1, 2, 4, 5, 6, 3]")
    diff(json1, json2) should be(
      parseJsonPatch("""[
                   |   {"op": "add", "path": "/-", "value": 1},
                   |   {"op": "add", "path": "/-", "value": 2},
                   |   {"op": "add", "path": "/-", "value": 3}
                   | ]""".stripMargin))
    diff(json2, json3) should be(
      parseJsonPatch("""[
                   |   {"op": "add", "path": "/2", "value": 4},
                   |   {"op": "add", "path": "/3", "value": 5},
                   |   {"op": "add", "path": "/4", "value": 6}
                   | ]""".stripMargin))
  }

  it should "contain a remove operation for each deleted element" in {
    val json1 = parseJson("[]")
    val json2 = parseJson("[1, 2, 3]")
    val json3 = parseJson("[1, 2, 4, 5, 6, 3]")
    diff(json2, json1) should be(
      parseJsonPatch("""[
                   |   {"op": "remove", "path": "/2"},
                   |   {"op": "remove", "path": "/1"},
                   |   {"op": "remove", "path": "/0"}
                   | ]""".stripMargin))
    diff(json3, json2) should be(
      parseJsonPatch("""[
                   |   {"op": "remove", "path": "/4"},
                   |   {"op": "remove", "path": "/3"},
                   |   {"op": "remove", "path": "/2"}
                   | ]""".stripMargin))
  }

  it should "contain a replace operation for each value that changed" in {
    val json1 = parseJson("[1, 2, 3]")
    val json2 = parseJson("[1, 2, 4]")
    val json3 = parseJson("[1, 6, 3]")
    val json4 = parseJson("""[1, {"a": 2}, 3]""")
    val json5 = parseJson("""[1, {"a": 7}, 3]""")
    diff(json1, json2) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "/2", "value": 4}
                   | ]""".stripMargin))
    diff(json1, json3) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "/1", "value": 6}
                   | ]""".stripMargin))
    diff(json4, json5) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "/1/a", "value": 7}
                   | ]""".stripMargin))
    diff(json4, json3) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "/1", "value": 6}
                   | ]""".stripMargin))
  }

  "applying a diff" should "be a fix point when applied to the first object used for the diff" in {
    val json1 = parseJson("""{"lbl": 32, "b": {"c": "gruik"}}""")
    val json2 = parseJson("""{"a": 3, "b": {"a": true, "b": 43}, "c": null}""")
    patch(json1, diff(json1, json2)) should be(json2)
  }

  "applying a diff to strings" should "provide a correct string representation" in {
    val json1 = parseJson("""{
                   |  "a": 1,
                   |  "b": true,
                   |  "c": "test"
                   |}""".stripMargin)
    val json2 = parseJson("""{"a":6,"c":"test2","d":false}""")
    val json3 = patch(json1, diff(json1, json2))
    json3 should be(json2)
  }

  "a remembering diff" should "correctly add removed values in array diffs" in {
    val json1 = parseJson("""{"lbl": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}""")
    val json2 = parseJson("""{"lbl": [1, 4, 5, 11, 6, 7]}""")
    diff(json1, json2) should be(JsonPatch(
      Remove[JsValue](Pointer("lbl", "2"), Some(jsonInt(3))),
      Remove[JsValue](Pointer("lbl", "1"), Some(jsonInt(2))),
      Add[JsValue](Pointer("lbl", "3"), jsonInt(11)),
      Remove[JsValue](Pointer("lbl", "8"), Some(jsonInt(10))),
      Remove[JsValue](Pointer("lbl", "7"), Some(jsonInt(9))),
      Remove[JsValue](Pointer("lbl", "6"), Some(jsonInt(8)))))
  }

  it should "correctly add removed values in object diffs" in {
    val json1 = parseJson("""{"a": 1, "b": true}""")
    val json2 = parseJson("""{"a": 1}""")
    diff(json1, json2) should be(JsonPatch(Remove[JsValue](Pointer("b"), Some(jsonBoolean(true)))))
  }

  it should "correctly add replaced values in object diffs" in {
    val json1 = parseJson("""{"a": 1, "b": false}""")
    val json2 = parseJson("""{"a": 1, "b": "test"}""")
    diff(json1, json2) should be(JsonPatch(Replace(Pointer("b"), jsonString("test"), Some(jsonBoolean(false)))))
  }

}
