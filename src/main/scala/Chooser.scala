/**
 * (C) 2014 King's College London, University of Warwick
 */

import scala.util.Random

// Utility functions based on random selection
object Chooser extends Random {
  // Selects a random value from a list (throws exception IndexOutOfBoundsException if the list is empty)
  def choose[V] (items: V*): V =
    items (nextInt (items.size))

  // Selects a random value from a list (throws exception IndexOutOfBoundsException if the list is empty)
  def chooseFrom[V] (all: Seq[V]): V =
    all (nextInt (all.size))

  // Tests a random number against the given probability, executing ifMet if passed, ifNotMet if not
  def ifHappens[V] (probability: Double) (ifMet: => V) (ifNotMet: => V): V =
    if (nextDouble <= probability) ifMet else ifNotMet

  // Tests a random number against the given probability, returning Some (ifMet) if passed, None if not
  def getIfHappens[V] (probability: Double) (ifMet: => V): Option[V] =
    ifHappens[Option[V]] (probability) (Some (ifMet)) (None)

  // Selects a random double between a minimum and maximum
  def randomDouble (minimum: Double, maximum: Double) =
    nextDouble * (maximum - minimum) + minimum

  // Selects a random integer between a minimum (inclusive) and maximum (exclusive)
  def randomInt (minimum: Int, maximum: Int) =
    nextInt (maximum - minimum) + minimum

  // Selects a random sample from a list up to a given number (ensuring no duplicates)
  def randomSubset[V] (all: Seq[V], number: Int): List[V] = number match {
    case 0 => Nil
    case n =>
      val shuffled = shuffle (all)
      shuffled.head :: randomSubset (shuffled, number - 1)
  }
}
