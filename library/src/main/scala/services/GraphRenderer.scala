package mycrossword
package services

import models.{Bounds, Filled, Grid, GridElement, Index}

import scalatags.Text.Frag

object GraphRenderer {
  import scalatags.Text.implicits._
  import scalatags.Text.svgTags._
  import scalatags.Text.svgAttrs._

  private def ELEMENT_LENGTH = 10

  def render(bounds: Bounds, grid: Grid): String = {
    // Fetch the grid contents
    val elements = for {
      r <- bounds.min.row to bounds.max.row
      c <- bounds.min.column until bounds.max.column
      i = Index(r, c)
    } yield {
      (i, grid(Index(r, c)))
    }
    svg(
      width := bounds.width * ELEMENT_LENGTH,
      height := bounds.height * ELEMENT_LENGTH
    )(
      elements.flatMap { case (i, el) => toSvg(el, i) }
    ).toString
  }

  private def toSvg(element: GridElement, index: Index): Option[Frag] =
    element match {
      case Filled(letter) =>
        Some(
          rect(
            x := index.column * ELEMENT_LENGTH,
            y := index.row * ELEMENT_LENGTH,
            width := ELEMENT_LENGTH,
            height := ELEMENT_LENGTH,
            strokeWidth := 3
          )
        )
      case _ => None
    }
}
