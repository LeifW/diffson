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

trait JsonProvider[JsValue] {

  def unapplyArray(value: JsValue): Option[Vector[JsValue]]

  def applyArray(elems: Vector[JsValue]): JsValue

  def unapplyObject(value: JsValue): Option[Map[String, JsValue]]

  def applyObject(fields: Map[String, JsValue]): JsValue

  val JsNull: JsValue

  def prettyPrint(value: JsValue): String

  def compactPrint(value: JsValue): String

  object JsArray {

    @inline
    def apply(elems: Vector[JsValue]): JsValue =
      applyArray(elems)

    @inline
    def unapply(value: JsValue): Option[Vector[JsValue]] =
      unapplyArray(value)

  }

  object JsObject {

    @inline
    def apply(fields: Map[String, JsValue]): JsValue =
      applyObject(fields)

    @inline
    def unapply(value: JsValue): Option[Map[String, JsValue]] =
      unapplyObject(value)

  }

}
