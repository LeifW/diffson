/*
* This file is part of the diffson project.
* Copyright (c) 2016 Lucas Satabin
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package gnieh.diffson

import cats.{ Apply, FlatMap }
import cats.implicits._
import io.circe._
import io.circe.Decoder.Result
import io.circe.syntax._

object circe extends CirceProvider

class CirceProvider extends JsonProvider[Json] {

  /*
  object DiffsonProtocol extends DiffsonProtocol

  trait DiffsonProtocol {

    implicit val pointerEncoder: Encoder[Pointer] =
      Encoder[String].contramap(_.serialize)

    implicit val pointerDecoder: Decoder[Pointer] =
      Decoder[String].emap { s =>
        Either.catchNonFatal(Pointer.parse(s))
          .leftMap(_.getMessage)
      }

    implicit val operationEncoder: Encoder[Operation] =
      Encoder.instance[Operation] {
        case Add(path, value) =>
          Json.obj(
            "op" -> Json.fromString("add"),
            "path" -> Json.fromString(path.serialize),
            "value" -> value)
        case Remove(path, None) =>
          Json.obj(
            "op" -> Json.fromString("remove"),
            "path" -> Json.fromString(path.serialize))
        case Remove(path, Some(old)) =>
          Json.obj(
            "op" -> Json.fromString("remove"),
            "path" -> Json.fromString(path.serialize),
            "old" -> old)
        case Replace(path, value, None) =>
          Json.obj(
            "op" -> Json.fromString("replace"),
            "path" -> Json.fromString(path.serialize),
            "value" -> value)
        case Replace(path, value, Some(old)) =>
          Json.obj(
            "op" -> Json.fromString("replace"),
            "path" -> Json.fromString(path.serialize),
            "value" -> value,
            "old" -> old)
        case Move(from, path) =>
          Json.obj(
            "op" -> Json.fromString("move"),
            "from" -> Json.fromString(from.serialize),
            "path" -> Json.fromString(path.serialize))
        case Copy(from, path) =>
          Json.obj(
            "op" -> Json.fromString("copy"),
            "from" -> Json.fromString(from.serialize),
            "path" -> Json.fromString(path.serialize))
        case Test(path, value) =>
          Json.obj(
            "op" -> Json.fromString("test"),
            "path" -> Json.fromString(path.serialize),
            "value" -> value)
      }

    implicit val operationDecoder: Decoder[Operation] =
      new Decoder[Operation] {

        private val A = Apply[Result]
        private val F = FlatMap[Result]

        override def apply(c: HCursor): Result[Operation] =
          F.flatMap(c.get[String]("op").leftMap(_.withMessage("missing 'op' field"))) {
            case "add" =>
              A.map2(c.get[Pointer]("path"), c.get[Json]("value"))(Add)
                .leftMap(_.withMessage("missing 'path' or 'value' field"))
            case "remove" =>
              A.map2(c.get[Pointer]("path"), c.get[Option[Json]]("old"))(Remove)
                .leftMap(_.withMessage("missing 'path' or 'old' field"))
            case "replace" =>
              A.map3(c.get[Pointer]("path"), c.get[Json]("value"), c.get[Option[Json]]("old"))(Replace)
                .leftMap(_.withMessage("missing 'path' or 'value' field"))
            case "move" =>
              A.map2(c.get[Pointer]("from"), c.get[Pointer]("path"))(Move)
                .leftMap(_.withMessage("missing 'from' or 'path' field"))
            case "copy" =>
              A.map2(c.get[Pointer]("from"), c.get[Pointer]("path"))(Copy)
                .leftMap(_.withMessage("missing 'from' or 'path' field"))
            case "test" =>
              A.map2(c.get[Pointer]("path"), c.get[Json]("value"))(Test)
                .leftMap(_.withMessage("missing 'path' or 'value' field"))
            case other =>
              Left(DecodingFailure(s"""Unknown operation "$other"""", c.history))
          }
      }

    implicit val jsonPatchEncoder: Encoder[JsonPatch[Json]] =
      Encoder[List[Json]].contramap(_.ops.map(_.asJson))

    implicit val jsonPatchDecoder: Decoder[JsonPatch[Json]] =
      new Decoder[JsonPatch[Json]] {

        private val F = FlatMap[Result]

        override def apply(c: HCursor): Result[JsonPatch[Json]] =
          F.flatMap(c.as[List[Json]]) { list =>
            F.map(list.traverse(_.as[Operation]))(JsonPatch[Json](_))
          }
      }
  }
  */

  val JsNull: Json =
    io.circe.Json.Null

  def applyArray(elems: Vector[Json]): Json =
    io.circe.Json.fromValues(elems)

  def applyObject(fields: Map[String, Json]): Json =
    io.circe.Json.fromFields(fields)

  def compactPrint(value: Json): String =
    value.noSpaces

  def prettyPrint(value: Json): String =
    value.spaces2

  def unapplyArray(value: Json): Option[Vector[Json]] =
    value.asArray

  def unapplyObject(value: Json): Option[Map[String, Json]] =
    value.asObject.map(_.toMap)
}
