package io.property

import scala.collection.mutable.ArrayBuffer
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import scala.reflect.ClassTag

object MacroPropertySet {
  def fieldImpl[T: c.WeakTypeTag](c: Context)(): c.Expr[Array[Property]] = {
    import c.universe._
    val propBase = c.typeOf[Property]

    val exprs = new ArrayBuffer[c.Tree]()

    val typeTag = implicitly[c.WeakTypeTag[T]]
    for (mem <- typeTag.tpe.members.toSeq.reverse) {
      if (mem.isTerm) {
        val term = mem.asTerm
        if (term.isVar) {
          val TypeRef(_, sym, _) = term.typeSignature
          val owner = sym.owner
          if (owner.companion.isClass) {
            val propType = owner.companion.asClass
            if (propType.baseClasses.exists(_.name.toString == "Property")) {

              val name = Constant(mem.name.toString)
              val nameExpr = term.name

              val get = term.getter
              val set = term.setter

              val realType = term.typeSignature.typeSymbol

              exprs += q"""
                new $propType($name.trim) {
                  override def get(self: PropertyContainer): $realType = self.asInstanceOf[$typeTag].$get
                  override def set(self: PropertyContainer, value: $realType): Unit = self.asInstanceOf[$typeTag].$set(value)
                }
              """

            }
          }
        }
      }
    }

    c.Expr[Array[Property]](q"Array[Property](..$exprs)")
  }

  def make[T](): Array[io.property.Property] = macro MacroPropertySet.fieldImpl[T]
}
