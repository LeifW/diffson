package gnieh.diffson
package jsonpatch
package test

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import scala.util.Try

import cats.implicits._

abstract class TestArrayDiff[JsValue](implicit val J: JsonProvider[JsValue]) extends Properties("TestArrayDiff") {

  import J._

  def jsonIntSeq(s: Seq[Int]): JsValue

  val dp = JsonDiff.provider[JsValue]()

  def diff(js1: JsValue, js2: JsValue): JsonPatch[JsValue] =
    dp.diff(js1, js2)

  def patch(js: JsValue, p: JsonPatch[JsValue]): JsValue =
    p[Try](js).get

  property("arrayDiff") = forAll {
    (a: Seq[Int], b: Seq[Int]) =>
      val jsa = jsonIntSeq(a)
      val jsb = jsonIntSeq(b)
      val p = diff(jsa, jsb)
      patch(jsa, p) == jsb
  }
}
