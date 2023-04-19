package nihonss

import java.io.PrintStream
import scala.annotation.unused
import scala.collection.mutable
import scala.quoted.*
import scala.sys.process
import scala.reflect.ClassTag
import scala.language.dynamics
import scala.annotation.tailrec
import scala.annotation.targetName

case class ImplicitS[S: ClassTag](s: S)
case class ImplicitO[O](o: O)
case class ImplicitTupO[O <: Tuple](o: O)

extension [S: ClassTag](subject: S)
  transparent inline infix def は[O](
      inline obj: O
  ): Sentence[S, O *: EmptyTuple] =
    subject match
      case r: AnyRef => Sentence(subject)(Tuple1(obj))

case class Sentence[S, O <: Tuple](subject: S)(obj: O):
  infix def と[O2](o2: O2): Sentence[S, Tuple.Append[O, O2]] =
    Sentence(subject)(obj :* o2)
  infix def を[R](@unused sugu: すぐ.type): DynamicImpl[S, O] =
    DynamicImpl(subject, obj)
  transparent inline infix def を(inline exprTree: Any): Any = ${
    macroImpl[S, O]('exprTree, 'subject, 'obj)
  }

def macroImpl[S, O <: Tuple](
    exprTree: Expr[Any],
    subject: Expr[S],
    obj: Expr[O]
)(using Quotes)(using Type[S])(using Type[O]): Expr[Any] =
  import quotes.reflect.*;
  exprTree.match
    case '{ すぐ.applyDynamic($method)(します) } =>
      appDynamicImpl(subject, obj, method)

object すぐ extends Dynamic {
  def applyDynamic(name: String)(args: します.type): Any =
    println(name)
}

object します

class DynamicImpl[S, O <: Tuple](subject: S, val obj: O) extends Dynamic {
  transparent inline def applyDynamic(inline name: String)(
      args: します.type
  ): Any =
    ${ appDynamicImpl('subject, 'obj, 'name) }
}

def appDynamicImpl[S, O <: Tuple](
    subject: Expr[S],
    obj: Expr[O],
    method: Expr[String]
)(using q: Quotes)(using Type[S])(using Type[O]): Expr[Any] =
  import quotes.reflect.*;
  println(obj)
  println(obj.show)
  println(obj.asTerm)
  println(Type.of[O])
  println(TypeRepr.of[O])
  val terms = unpackList(obj)
  println(terms)
  val selected = Select.overloaded(
    subject.asTerm,
    method.valueOrAbort,
    List(),
    terms
  )
  selected.asExpr

transparent inline private def unpackList[T <: Tuple](using
    q: Quotes
)(inline x: Expr[T])(using
    Type[T]
): List[q.reflect.Term] =
  import q.reflect.*
  println(x)
  println(x.show)
  println(x.asTerm)
  println(Type.of[T])
  println(TypeRepr.of[T])
  x match {
    case '{ EmptyTuple }                  => Nil
    case '{ List($x1) }                   => List(x1.asTerm)
    case '{ _root_.scala.Tuple1[T]($x1) } => List(x1.asTerm)
    case '{ ($x1, $x2) }                  => List(x1.asTerm, x2.asTerm)
    // case '{ ($head: h) *: ($tail: Tuple) } =>
    //   head.asTerm :: unpackList(tail)
  }
