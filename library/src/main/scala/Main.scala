package mycrossword

import services.CrosswordBuilderService

import mycrossword.models.Word

object Main extends App {
  val words = Seq(
    "adana",
    "Breakwater",
    "Brentford",
    "Dags",
    "laboca",
    "Lajares",
    "Malverde",
    "Mao",
    "Marinara",
    "Maud",
    "oscar",
    "Plan",
    "Quarry",
    "Rats",
    "Ron",
    "Rooibos",
    "Tumnus",
    "watermelon",
    "Zidane"
  )
  val service = new CrosswordBuilderService()
  val crossword = service.findGoodCrossword(
    words.map(Word.apply).toSet,
    1000
  )
  println(crossword.repr())
}
