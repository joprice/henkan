//Adapted from simulacrum
// https://github.com/mpilquist/simulacrum/blob/master/core/src/main/scala/simulacrum/typeclass.scala
package henkan.extension

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

//@compileTimeOnly("newMonadType annotation should have been removed by simulacrum but was not")
class newMonadType extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro NewMonadTypeMacros.generateTypeClass
}

class NewMonadTypeMacros(val c: Context) {

  import c.universe._
  def generateTypeClass(annottees: c.Expr[Any]*): c.Expr[Any] = {

    def abort(msg: String = "@newMonadType can only be applied to case classes that take a single type parameter and one field whose type has a Monad") =
      c.abort(c.enclosingPosition, msg)

    def modify(newType: ClassDef, companion: Tree) = {
      val tparam = newType.tparams match {
        case hd :: Nil ⇒
          hd.tparams.size match {
            case 1 ⇒ hd
            case n ⇒ abort()
          }
        case other ⇒ abort()
      }

      val primaryConstructor = newType.tpe.decls.collectFirst {
        case m if m.isMethod && m.asMethod.isPrimaryConstructor =>
          m.asMethod
      }.getOrElse {
        abort(s"$newType doesn't seem to be a case class")
      }

      val modifiedCompanion = {
        val q"$mods object $name extends ..$bases { ..$body }" = companion
        q"""
          $mods object $name extends ..$bases {
            ..$body
          }
        """
      }

      val result = c.Expr(q"""
        $newType
        $modifiedCompanion
      """)

      c.info(c.enclosingPosition, s"Generated type class ${newType.name}:\n" + showCode(result.tree), false)

      result
    }

    annottees.map(_.tree) match {
      case (newType: ClassDef) :: Nil                           ⇒ modify(newType, q"object ${newType.name.toTermName} {}")
      case (newType: ClassDef) :: (companion: ModuleDef) :: Nil ⇒ modify(newType, companion)
      case other :: Nil                                         ⇒ abort()
    }

  }
}

@newMonadType
case class TestMe[T](a: Option[T])
