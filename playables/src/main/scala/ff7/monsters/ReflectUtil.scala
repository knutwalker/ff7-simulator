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
package monsters

import scalaz._, Scalaz._
import scala.reflect.runtime.{universe ⇒ ru}

object ReflectUtil {

  private val loader = getClass.getClassLoader
  private val m = ru.runtimeMirror(loader)

  def loadObject[A: ru.TypeTag](name: String): String \/ A = for {
    sym  ← loadObjectSym(name)
    inst ← objectInstance[A](sym)
  } yield inst

  private def loadObjectSym(name: String): String \/ ru.ModuleSymbol =
    for {
      target ← loadClass(name + "$")
      symbol ← objectSymbol(target)
    } yield symbol

  private def objectInstance[A: ru.TypeTag](sym: ru.ModuleSymbol): String \/ A = {
    val targetTpe = ru.typeOf[A]
    if (sym.typeSignature <:< targetTpe) for {
      obj ← TryE(m.reflectModule(sym))
      ins ← TryE(obj.instance.asInstanceOf[A])
    } yield ins
    else s"[$sym] is not <:< [$targetTpe]".left
  }

  private def loadClass(name: String): String \/ Class[_] =
    try {
      loader.loadClass(name).right
    } catch {
      case e: ClassNotFoundException ⇒
        s"The class [$name] could not be found".left
    }

  private def objectSymbol(clazz: Class[_]): String \/ ru.ModuleSymbol =
    TryE(m.moduleSymbol(clazz))
}
