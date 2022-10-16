package mycrossword
package services

import models.{Blocked, Bounds, Filled, Grid, GridElement, Index}

import scalatags.Text.Frag

object GraphRenderer {
  import scalatags.Text.implicits._
  import scalatags.Text.svgTags._
  import scalatags.Text.svgAttrs._

  private def ELEMENT_LENGTH = 30

  def render(bounds: Bounds, grid: Grid): String = {
    // Fetch the grid contents
    val elements = for {
      r <- bounds.min.row to bounds.max.row
      c <- bounds.min.column until bounds.max.column
      i = Index(r, c)
    } yield {
      (i - bounds.min, grid(Index(r, c)))
    }
    svg(
      width := bounds.width * ELEMENT_LENGTH,
      height := bounds.height * ELEMENT_LENGTH,
      style := "background-color:rgb(220,220,220)"
    )(
      elements.flatMap { case (i, el) => toSvg(el, i) }
    ).toString
  }

  private def toSvg(element: GridElement, index: Index): Option[Frag] =
    val xPos = index.column * ELEMENT_LENGTH
    val yPos = index.row * ELEMENT_LENGTH
    element match {
      case Filled(letter) =>
        Some(
          g(
            rect(
              x := xPos,
              y := yPos,
              width := ELEMENT_LENGTH,
              height := ELEMENT_LENGTH,
              fill := "white",
              stroke := "black",
              strokeWidth := 3
            ),
            text(
              x := xPos + ELEMENT_LENGTH / 2,
              y := yPos + ELEMENT_LENGTH / 2,
              dominantBaseline := "middle",
              textAnchor := "middle",
              fontSize := "30px",
              letter.char.toUpper.toString
            )
          )
        )
      case _ => None
    }
}
