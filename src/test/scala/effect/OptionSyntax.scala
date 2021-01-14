package es.eriktorr
package effect

trait OptionSyntax {
  implicit class OptionOps[A](self: Option[A]) {
    def stringOrEmpty: String = self.map(_.toString).getOrElse("")
  }
}
