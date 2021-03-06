package tests
package typesSignatures

class A
{
  type A = Int
  type B[+T] = Seq[T]
  type C[A, B <: A] = Seq[B]
}

trait V 
{
  type Ala[+J] <: Int
  type Ola[+T]
  type X
} 

class Generic[T]

class Base
{
  type A
  type B = Int

  // Tests not support multiline signatures
  type MatchT[T] = T match { case String => Char case Int => Byte }

  // Tests do not support multiline signatures
  type Elem[X] = X match { case String => Char case Array[t] => t case Iterable[t] => t }

  type F = PolyFunction { def apply[X](x: X): List[X] }
}