package mycrossword
package models

case class CandidateLookup(lookup: Map[Char, Set[Index]]) {
  def remove(char: Char, point: Index): CandidateLookup =
    val removedSet = lookup(char).filterNot(_ == point)
    CandidateLookup(lookup + (char -> removedSet))

  def get(char: Char): Set[Index] = lookup(char)

  def append(char: Char, point: Index): CandidateLookup = {
    val current = lookup(char)
    CandidateLookup(lookup.updated(char, current + point))
  }
}

object CandidateLookup {
  def create(): CandidateLookup = CandidateLookup(Map.empty.withDefaultValue(Set.empty))
  def create(lookup: Map[Char, Set[Index]]): CandidateLookup = CandidateLookup(lookup.withDefaultValue(Set.empty))
}