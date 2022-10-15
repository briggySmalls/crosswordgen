package mycrossword

import services.CrosswordBuilderService

import mycrossword.models.Word

object Main extends App {
  val service = new CrosswordBuilderService()
  val crossword = service.buildCrossword(
    Set(
      Word("analogue", "nonsense"),
      Word("farmyard", "nonsense"),
      Word("brentford", "nonsense"),
      Word("dizzy", "nonsense"),
      Word("round", "nonsense")
    )
  )
  println(crossword.repr())
}
