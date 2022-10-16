package mycrossword

import services.CrosswordBuilderService

import mycrossword.models.Word

object Main extends App {
  val words = Map(
    "adana" -> "Turkish city, Minced lamb (5)",
    "Breakwater" -> "Sea wall (10)",
    "Brentford" -> "By far the greatest team (9)",
    "Dags" -> "Endearing idiots, matted sheep hair (4)",
    "laboca" -> "Reggaeton song (2,4)",
    "Lajares" -> "Charming goat village (7)",
    "Malverde" -> "Vendor of the finest prawn tacos (8)",
    "Mao" -> "Domestic worker with erratic schedule (3)",
    "Marinara" -> "Donal’s favourite pizza (8)",
    "Maud" -> "Little warrior (4)",
    "oscar" -> "‘O’ in phonetic alphabet ",
    "Plan" -> "Make arrangements for (4)",
    "Quarry" -> "Open-air dance event (6)",
    "Rats" -> "“Commune ate my chocolate” (4)",
    "Ron" -> "Literary character, trivia game (3)",
    "Rooibos" -> "Delicious beverage (7)",
    "Tumnus" -> "Stomach (informal) (6)",
    "watermelon" -> "Fruit for hangover (10)",
    "Zidane" -> "The strongest of them all (6)"
  )
  val service = new CrosswordBuilderService()
  val crossword = service.findGoodCrossword(
    words.map { case (word, clue) => Word(word, clue) }.toSet,
    1000
  )
  println(crossword.repr())
}
