/*
* This file is part of the diffson project.
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
package jsonpatch

import cats._
import cats.implicits._

import scala.collection.immutable.VectorBuilder

import scala.annotation.tailrec

import scala.language.higherKinds

object JsonPatch {

  def apply[JsValue](ops: Operation[JsValue]*): JsonPatch[JsValue] =
    new JsonPatch(ops.toList)

  implicit def JsonPatchPatch[JsValue](implicit J: JsonProvider[JsValue]): PatchProvider[JsValue, JsonPatch] =
    new PatchProvider[JsValue, JsonPatch] {

      def patch[F[_]](json: JsValue, p: JsonPatch[JsValue])(implicit F: MonadError[F, Throwable]): F[JsValue] =
        p[F](json)

    }

}

/** A Json patch object according to http://tools.ietf.org/html/rfc6902
 *
 *  @author Lucas Satabin
 */
case class JsonPatch[JsValue](ops: List[Operation[JsValue]]) {
  self =>

  class WithFilter(p: Operation[JsValue] => Boolean) {

    def map(f: Operation[JsValue] => Operation[JsValue]): JsonPatch[JsValue] =
      self.flatMap(op => if (p(op)) JsonPatch(f(op)) else JsonPatch())

    def flatMap(f: Operation[JsValue] => JsonPatch[JsValue]): JsonPatch[JsValue] =
      self.flatMap(op => if (p(op)) f(op) else JsonPatch())

    def withFilter(p2: Operation[JsValue] => Boolean): WithFilter =
      new WithFilter(op => p(op) && p2(op))

    def foreach(f: Operation[JsValue] => Unit): Unit =
      self.foreach(op => if (p(op)) f(op))

  }

  def apply[F[_]](json: JsValue)(implicit F: MonadError[F, Throwable], J: JsonProvider[JsValue]): F[JsValue] =
    ops.foldLeftM[F, JsValue](json)((acc, op) => op[F](acc))

  /** Create a patch that applies `this` patch and then `that` patch */
  def andThen(that: JsonPatch[JsValue]): JsonPatch[JsValue] =
    new JsonPatch(this.ops ++ that.ops)

  def map(f: Operation[JsValue] => Operation[JsValue]): JsonPatch[JsValue] =
    JsonPatch(ops.map(f))

  def flatMap(f: Operation[JsValue] => JsonPatch[JsValue]): JsonPatch[JsValue] =
    JsonPatch(for {
      op <- ops
      JsonPatch(ops) = f(op)
      op <- ops
    } yield op)

  def filter(p: Operation[JsValue] => Boolean): JsonPatch[JsValue] =
    JsonPatch(ops.filter(p))

  def withFilter(p: Operation[JsValue] => Boolean): WithFilter =
    new WithFilter(p)

  def foldLeft[Res](zero: Res)(f: (Res, Operation[JsValue]) => Res): Res =
    ops.foldLeft(zero)(f)

  def foldRight[Res](zero: Res)(f: (Operation[JsValue], Res) => Res): Res =
    ops.foldRight(zero)(f)

  def foreach(f: Operation[JsValue] => Unit): Unit =
    ops.foreach(f)

  def collect[T](pf: PartialFunction[Operation[JsValue], T]): Seq[T] =
    ops.collect(pf)

}

/** A patch operation to apply to a Json value */
sealed trait Operation[JsValue] {
  val path: Pointer

  /** Applies this operation to the given Json value */
  def apply[F[_]](value: JsValue)(implicit F: MonadError[F, Throwable], J: JsonProvider[JsValue]): F[JsValue] =
    action[F](value, path.path, Pointer.Root)

  // the action to perform in this operation. By default this is a really inefficient `identity` function.
  protected[this] def action[F[_]](value: JsValue, pointer: Pointer, parent: Pointer)(implicit F: MonadError[F, Throwable], J: JsonProvider[JsValue]): F[JsValue] = {
    import J._
    (value, pointer) match {
      case (_, Pointer.Root) =>
        F.pure(value)
      case (JsObject(fields), ObjectField(elem) +: tl) if fields.contains(elem) =>
        val patched =
          fields.get(elem) match {
            case Some(value) =>
              action[F](value, tl, parent :+ Left(elem)).map(fields.updated(elem, _))
            case None =>
              F.pure(fields)
          }
        patched.map(JsObject(_))
      case (JsArray(elems), ArrayIndex(idx) +: tl) =>
        if (idx >= elems.size) {
          F.raiseError(new PatchException(s"element $idx does not exist at path ${parent.serialize}"))
        } else {
          val (before, after) = elems.splitAt(idx)
          val builder = new VectorBuilder[JsValue]
          builder.sizeHint(elems.size)
          builder ++= before
          action[F](elems(idx), tl, Right(idx) +: parent).map { patched =>
            builder += patched
            builder ++= after.view(1, after.size)
            JsArray(builder.result)
          }
        }
      case (_, elem +: _) =>
        throw new PatchException(s"element ${elem.fold(identity, _.toString)} does not exist at path ${parent.serialize}")
    }
  }

  protected object ArrayIndex {
    def unapply(e: Part): Option[Int] = e.toOption
  }

  protected object ObjectField {
    def unapply(e: Part): Option[String] = Some(e.fold(identity, _.toString))
  }

}

/** Add (or replace if existing) the pointed element */
case class Add[JsValue](path: Pointer, value: JsValue) extends Operation[JsValue] {

  override protected[this] def action[F[_]](original: JsValue, pointer: Pointer, parent: Pointer)(implicit F: MonadError[F, Throwable], J: JsonProvider[JsValue]): F[JsValue] = {
    import J._
    (original, pointer.path) match {
      case (_, Pointer.Root) =>
        // we are at the root value, simply return the replacement value
        F.pure(value)
      case (JsArray(arr), Pointer(Left("-"))) =>
        // insert the value at the end of the array
        F.pure(JsArray(arr :+ value))
      case (JsArray(arr), Pointer(ArrayIndex(idx))) =>
        if (idx > arr.size) {
          F.raiseError(new PatchException(s"element $idx does not exist at path ${parent.serialize}"))
        } else {
          // insert the value at the specified index
          val (before, after) = arr.splitAt(idx)
          val builder = new VectorBuilder[JsValue]
          builder.sizeHint(arr.size + 1)
          builder ++= before
          builder += value
          builder ++= after
          F.pure(JsArray(builder.result))
        }
      case (JsObject(obj), Pointer(ObjectField(lbl))) =>
        // insert the new label
        F.pure(JsObject(obj.updated(lbl, value)))
      case _ =>
        super.action[F](original, pointer, parent)
    }
  }

}

/** Remove the pointed element */
case class Remove[JsValue](path: Pointer, old: Option[JsValue] = None) extends Operation[JsValue] {

  override protected[this] def action[F[_]](original: JsValue, pointer: Pointer, parent: Pointer)(implicit F: MonadError[F, Throwable], J: JsonProvider[JsValue]): F[JsValue] = {
    import J._
    (original, pointer.path) match {
      case (JsArray(arr), Pointer(ArrayIndex(idx))) =>
        if (idx >= arr.size) {
          // we know thanks to the extractor that the index cannot be negative
          F.raiseError(new PatchException(s"element $idx does not exist at path ${parent.serialize}"))
        } else {
          // remove the element at the given index
          val (before, after) = arr.splitAt(idx)
          F.pure(JsArray(before ++ after.tail))
        }
      case (JsArray(_), Pointer(Left("-"))) =>
        // how could we possibly remove an element that appears after the last one?
        F.raiseError(new PatchException(s"element - does not exist at path ${parent.serialize}"))
      case (JsObject(obj), Pointer(ObjectField(lbl))) if obj.contains(lbl) =>
        // remove the field from the object if present, otherwise, ignore it
        F.pure(JsObject(obj - lbl))
      case (_, Pointer.Root) =>
        throw new PatchException("Cannot delete an empty path")
      case _ =>
        super.action[F](original, pointer, parent)
    }
  }

}

/** Replace the pointed element by the given value */
case class Replace[JsValue](path: Pointer, value: JsValue, old: Option[JsValue] = None) extends Operation[JsValue] {

  override protected[this] def action[F[_]](original: JsValue, pointer: Pointer, parent: Pointer)(implicit F: MonadError[F, Throwable], J: JsonProvider[JsValue]): F[JsValue] = {
    import J._
    (original, pointer.path) match {
      case (_, Pointer.Root) =>
        // simply replace the root value by the replacement value
        F.pure(value)
      case (JsArray(arr), Pointer(Right(idx))) =>
        if (idx >= arr.size) {
          F.raiseError(new PatchException(s"element $idx does not exist at path ${parent.serialize}"))
        } else {
          val (before, after) = arr.splitAt(idx)
          val builder = new VectorBuilder[JsValue]
          builder.sizeHint(arr.size)
          builder ++= before
          builder += value
          builder ++= after.view(1, after.size)
          F.pure(JsArray(builder.result))
        }
      case (JsArray(_), Pointer(Left("-"))) =>
        F.raiseError(new PatchException(s"element - does not exist at path ${parent.serialize}"))
      case (JsObject(obj), Pointer(ObjectField(lbl))) =>
        if (obj.contains(lbl)) {
          F.pure(JsObject(obj.updated(lbl, value)))
        } else {
          F.raiseError(new PatchException(s"element $lbl does not exist at path ${parent.serialize}"))
        }
      case _ =>
        super.action[F](original, pointer, parent)
    }
  }

}

/** Move the pointed element to the new position */
case class Move[JsValue](from: Pointer, path: Pointer) extends Operation[JsValue] {

  override def apply[F[_]](original: JsValue)(implicit F: MonadError[F, Throwable], J: JsonProvider[JsValue]): F[JsValue] = {
    @tailrec
    def prefix(p1: Pointer, p2: Pointer): Boolean = (p1.path, p2.path) match {
      case (h1 +: tl1, h2 +: tl2) if h1 == h2 => prefix(tl1, tl2)
      case (Pointer.Root, _ +: _)             => true
      case (_, _)                             => false
    }
    if (prefix(from, path))
      F.raiseError(new PatchException("The destination path cannot be a descendant of the source path"))
    else
      for {
        cleaned <- Remove(from).apply[F](original)
        value <- from.evaluate[F, JsValue](original)
        added <- Add(path, value).apply[F](cleaned)
      } yield added
  }
}

/** Copy the pointed element to the new position */
case class Copy[JsValue](from: Pointer, path: Pointer) extends Operation[JsValue] {

  override def apply[F[_]](original: JsValue)(implicit F: MonadError[F, Throwable], J: JsonProvider[JsValue]): F[JsValue] =
    for {
      value <- from.evaluate[F, JsValue](original)
      patched <- Add(path, value).apply[F](original)
    } yield patched

}

/** Test that the pointed element is equal to the given value */
case class Test[JsValue](path: Pointer, value: JsValue) extends Operation[JsValue] {

  override def apply[F[_]](original: JsValue)(implicit F: MonadError[F, Throwable], J: JsonProvider[JsValue]): F[JsValue] =
    for {
      orig <- path.evaluate[F, JsValue](original)
      _ <- if (value != orig)
        F.raiseError(new PatchException(s"test failed at path ${path.serialize}"))
      else
        F.pure(())
    } yield original
}
