/*
* This file is part of the diffson project.
* Copyright (c) 2018 Lucas Satabin
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

import cats._

import scala.collection.immutable.Queue

import scala.language.higherKinds

/** This package contains an implementation of Json JsonPatch, according to [RFC-6902](http://tools.ietf.org/html/rfc6902)
 */
package object jsonpatch {

  type Part = Either[String, Int]
  type Pointer = Queue[Part]

  object Pointer {
    val Root: Pointer = Queue.empty

    private val IsDigit = "(0|[1-9][0-9]*)".r

    def apply(elems: String*): Pointer = Queue(elems.map {
      case IsDigit(idx) => Right(idx.toInt)
      case key          => Left(key)
    }: _*)

    def unapplySeq(pointer: Pointer): Option[Queue[Part]] = Queue.unapplySeq(pointer)

    /** Parses a JSON pointer and returns the resolved path. */
    def parse[F[_]](input: String)(implicit F: MonadError[F, Throwable]): F[Pointer] =
      if (input == null || input.isEmpty)
        // shortcut if input is empty
        F.pure(Pointer.Root)
      else if (!input.startsWith("/")) {
        // a pointer MUST start with a '/'
        F.raiseError(new PointerException("A JSON pointer must start with '/'"))
      } else {
        // first gets the different parts of the pointer
        val parts = input.split("/")
          // the first element is always empty as the path starts with a '/'
          .drop(1)
        if (parts.length == 0) {
          // the pointer was simply "/"
          F.pure(Pointer(""))
        } else {
          // check that an occurrence of '~' is followed by '0' or '1'
          if (parts.exists(_.matches(".*~(?![01]).*"))) {
            F.raiseError(new PointerException("Occurrences of '~' must be followed by '0' or '1'"))
          } else {
            val allParts = if (input.endsWith("/")) parts :+ "" else parts

            val elems = allParts
              // transform the occurrences of '~1' into occurrences of '/'
              // transform the occurrences of '~0' into occurrences of '~'
              .map(_.replace("~1", "/").replace("~0", "~"))
            F.pure(Pointer(elems: _*))
          }
        }
      }
  }

  implicit class PointerOps(val path: Pointer) extends AnyVal {

    def /(key: String): Pointer = path :+ Left(key)

    def /(idx: Int): Pointer = path :+ Right(idx)

    def serialize: String =
      if (path.isEmpty)
        ""
      else
        "/" + path.map {
          case Left(l)  => l.replace("~", "~0").replace("/", "~1")
          case Right(r) => r.toString
        }.mkString("/")

    /** Evaluates the given path in the given JSON object.
     *  Upon missing elements in value, the error handler is called with the current value and element
     */
    def evaluate[F[_], JsValue](value: JsValue)(implicit F: MonadError[F, Throwable], provider: JsonProvider[JsValue]): F[JsValue] = {
      import provider._
      F.tailRecM((value, path, Pointer.Root)) {
        case (JsObject(obj), Left(elem) +: tl, parent) =>
          F.pure(Left((obj.getOrElse(elem, JsNull), tl, parent :+ Left(elem))))
        case (JsArray(arr), Right(idx) +: tl, parent) =>
          if (idx >= arr.size)
            F.raiseError(new PointerException(s"element '$idx' does not exist at path ${parent.serialize}"))
          else
            F.pure(Left((arr(idx), tl, parent :+ Right(idx))))
        case (arr @ JsArray(_), Left("-") +: tl, parent) =>
          F.raiseError(new PointerException(s"element '-' does not exist at path ${parent.serialize}"))
        case (_, Pointer.Root, _) =>
          F.pure(Right(value))
        case (_, Left(elem) +: tl, parent) =>
          F.raiseError(new PointerException(s"element '$elem' does not exist at path ${parent.serialize}"))
      }
    }

  }

}
