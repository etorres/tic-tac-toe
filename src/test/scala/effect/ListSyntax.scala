package es.eriktorr
package effect

trait ListSyntax {
  implicit class ListOps[A](self: List[A]) {
    def intersperse(xs: List[A]): List[A] =
      self
        .map(List(_))
        .zipAll(xs.map(List(_)), Nil, Nil)
        .flatMap(Function.tupled(_ ::: _))
  }
}
