/*
 * Copyright 2015 Paul Horn
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ff7

import scalaz._, Free._

package object algebra {

  private[algebra] implicit def liftFree[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): FreeC[G, A] =
    Free.liftFC(I.inj(fa))

  private[algebra] def freePoint[F[_], A](a: A): FreeC[F, A] =
    Free.point[({type λ[α] = Coyoneda[F, α]})#λ, A](a)

  implicit class OrTransform[F[_], G[_]](val f: F ~> G) extends AnyVal {
    def or[H[_]](g: H ~> G): ({type λ[α] = Coproduct[F, H, α]})#λ ~> G =
      new (({type λ[α] = Coproduct[F, H, α]})#λ ~> G) {
        def apply[A](fa: Coproduct[F, H, A]): G[A] =
          fa.run.fold(f.apply, g.apply)
      }
  }
}
