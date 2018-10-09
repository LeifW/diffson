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

import cats._
import cats.implicits._

import scala.language.higherKinds

/** A provider for patch types and operations.
 *  The provided `Patch` must operate on instances of `JsValue`.
 */
trait PatchProvider[JsValue, Patch[_]] {

  /** Applies a patch to the given Json value, and returns the patched value. */
  def patch[F[_]](json: JsValue, p: Patch[JsValue])(implicit F: MonadError[F, Throwable]): F[JsValue]

}
