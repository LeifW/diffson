package gnieh.diffson
package jsonpatch
package test

import cats.implicits._

import org.scalatest._

abstract class TestSimpleDiff[JsValue](implicit val J: JsonProvider[JsValue]) extends FlatSpec with Matchers {

  import J._

  val dp = JsonDiff.provider[JsValue](arrayDiffs = false)
  val dpo = JsonDiff.provider[JsValue](arrayDiffs = false, remember = true)

  def simpleDiff(js1: JsValue, js2: JsValue) =
    dp.diff(js1, js2)

  def simpleDiffOld(js1: JsValue, js2: JsValue) =
    dpo.diff(js1, js2)

  def jsonBoolean(b: Boolean): JsValue
  def jsonInt(i: Int): JsValue
  def jsonString(s: String): JsValue
  def parseJson(s: String): JsValue
  def parseJsonPatch(s: String): JsonPatch[JsValue]

  "a diff" should "be empty if created between two equal values" in {
    val json = jsonBoolean(true)
    simpleDiff(json, json) should be(JsonPatch(Nil))
  }

  it should "be a simple replacement if the two values are completely different" in {
    simpleDiff(jsonBoolean(true), jsonInt(13)) should be(JsonPatch(Replace(Pointer.Root, jsonInt(13))))
  }

  it should "contain an add operation for each added field" in {
    val json1 = parseJson("""{"lbl": 32}""")
    val json2 = parseJson("""{"lbl": 32, "new": false}""")
    val json3 = parseJson("""{"lbl": 32, "new1": false, "new2": null}""")
    val json4 = parseJson("""{"a": 3, "b": {"a": true }}""")
    val json5 = parseJson("""{"a": 3, "b": {"a": true, "b": 43}, "c": null}""")
    simpleDiff(json1, json2) should be(JsonPatch(Add[JsValue](Pointer("new"), jsonBoolean(false))))
    simpleDiff(json1, json3) should be(JsonPatch(Add[JsValue](Pointer("new2"), JsNull), Add[JsValue](Pointer("new1"), jsonBoolean(false))))
    simpleDiff(json4, json5) should be(JsonPatch(Add[JsValue](Pointer("b", "b"), jsonInt(43)), Add[JsValue](Pointer("c"), JsNull)))
  }

  it should "contain a remove operation for each removed field" in {
    val json1 = parseJson("""{"lbl": 32}""")
    val json2 = parseJson("""{"lbl": 32, "old": false}""")
    val json3 = parseJson("""{"lbl": 32, "old1": false, "old2": null}""")
    val json4 = parseJson("""{"a": 3, "b": {"a": true }}""")
    val json5 = parseJson("""{"a": 3, "b": {"a": true, "b": 43}, "c": null}""")
    simpleDiff(json2, json1) should be(JsonPatch(Remove[JsValue](Pointer("old"))))
    simpleDiff(json3, json1) should be(JsonPatch(Remove[JsValue](Pointer("old2")), Remove[JsValue](Pointer("old1"))))
    simpleDiff(json5, json4) should be(JsonPatch(Remove[JsValue](Pointer("b", "b")), Remove[JsValue](Pointer("c"))))
  }

  it should "correctly handle array diffs in objects (i.e. just replaced)" in {
    val json1 = parseJson("""{"lbl": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}""")
    val json2 = parseJson("""{"lbl": [1, 4, 5, 11, 6, 7]}""")
    simpleDiff(json1, json2) should be(JsonPatch(Replace(Pointer("lbl"), JsArray(Vector(jsonInt(1), jsonInt(4), jsonInt(5), jsonInt(11), jsonInt(6), jsonInt(7))))))
  }

  it should "contain a replace operation for each changed field value" in {
    val json1 = parseJson("""{"lbl": 32}""")
    val json2 = parseJson("""{"lbl": 60}""")
    val json3 = parseJson("""{"lbl": {"a": true}}""")
    val json4 = parseJson("""{"lbl": {"a": null}}""")
    simpleDiff(json1, json2) should be(JsonPatch(Replace(Pointer("lbl"), jsonInt(60))))
    simpleDiff(json1, json3) should be(JsonPatch(Replace(Pointer("lbl"), parseJson("""{"a": true}"""))))
    simpleDiff(json3, json4) should be(JsonPatch(Replace(Pointer("lbl", "a"), JsNull)))
  }

  it should "contain a replaced operation for the changed array (additions)" in {
    val json1 = parseJson("[]")
    val json2 = parseJson("[1, 2, 3]")
    val json3 = parseJson("[1, 2, 4, 5, 6, 3]")
    simpleDiff(json1, json2) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "", "value": [1, 2, 3]}
                   | ]""".stripMargin))
    simpleDiff(json2, json3) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "", "value": [1, 2, 4, 5, 6, 3]}
                   | ]""".stripMargin))
  }

  it should "contain a replaced operation for the changed array (deletions)" in {
    val json1 = parseJson("[]")
    val json2 = parseJson("[1, 2, 3]")
    val json3 = parseJson("[1, 2, 4, 5, 6, 3]")
    simpleDiff(json2, json1) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "", "value": []}
                   | ]""".stripMargin))
    simpleDiff(json3, json2) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "", "value": [1, 2, 3]}
                   | ]""".stripMargin))
  }

  it should "contain a replace operation for the entire array if at least one element in it changed" in {
    val json1 = parseJson("[1, 2, 3]")
    val json2 = parseJson("[1, 2, 4]")
    val json3 = parseJson("[1, 6, 3]")
    val json4 = parseJson("""[1, {"a": 2}, 3]""")
    val json5 = parseJson("""[1, {"a": 7}, 3]""")
    simpleDiff(json1, json2) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "", "value": [1, 2, 4]}
                   | ]""".stripMargin))
    simpleDiff(json1, json3) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "", "value": [1, 6, 3]}
                   | ]""".stripMargin))
    simpleDiff(json4, json5) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "", "value": [1, {"a": 7}, 3]}
                   | ]""".stripMargin))
    simpleDiff(json4, json3) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "", "value": [1, 6, 3]}
                   | ]""".stripMargin))
  }

  "applying a diff" should "be a fix point when applied to the first object used for the diff" in {
    val json1 = parseJson("""{"lbl": 32, "b": {"c": "gruik"}}""")
    val json2 = parseJson("""{"a": 3, "b": {"a": true, "b": 43}, "c": null}""")
    simpleDiff(json1, json2)(json1) should be(json2)
  }

  "applying a diff to strings" should "provide a correct string representation" in {
    val json1 = parseJson("""{
                   |  "a": 1,
                   |  "b": true,
                   |  "c": "test"
                   |}""".stripMargin)
    val json2 = parseJson("""{"a":6,"c":"test2","d":false}""")
    val json3 = simpleDiff(json1, json2)(json1)
    json3 should be(json2)
  }

  "a remembering diff" should "correctly remember old value array" in {
    val json1 = parseJson("""{"lbl": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}""")
    val json2 = parseJson("""{"lbl": [1, 4, 5, 11, 6, 7]}""")
    simpleDiffOld(json1, json2) should be(
      parseJsonPatch("""[
                   |   {"op": "replace", "path": "/lbl", "value": [1, 4, 5, 11, 6, 7], "old": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}
                   | ]""".stripMargin))
  }

  it should "correctly add removed values in object diffs" in {
    val json1 = parseJson("""{"a": 1, "b": true}""")
    val json2 = parseJson("""{"a": 1}""")
    simpleDiffOld(json1, json2) should be(JsonPatch(Remove[JsValue](Pointer("b"), Some(jsonBoolean(true)))))
  }

  it should "correctly add replaced values in object diffs" in {
    val json1 = parseJson("""{"a": 1, "b": false}""")
    val json2 = parseJson("""{"a": 1, "b": "test"}""")
    simpleDiffOld(json1, json2) should be(JsonPatch(Replace(Pointer("b"), jsonString("test"), Some(jsonBoolean(false)))))
  }

}
