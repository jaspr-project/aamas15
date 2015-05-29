/**
 * (C) 2014 King's College London, University of Warwick
 */
import java.text.SimpleDateFormat
import java.util.Date
import Chooser.{chooseFrom, randomDouble}
import Configuration._
import Results.{record, cumulativeUtilities, oneSimUtilities}
import ResultsAccess.{loadAll, writeAll, writeAverages, writeOneSimulation}
import Utilities.average

// The program root application, which simulates each strategy and outputs cumulative utility
object Simulation extends App {
  // Perform one simulation of each of the set of strategies, using the same network configuration
  def simulateStrategies (simulation: Int) {
    val network = Network ()
    import network._

    // Perform one simulation of one strategy
    def simulateStrategy (strategy: Strategy) {
      // Perform one round of the simulation
      def tick (round: Int) {
        // Perform a set of client-provider interactions, selecting the provider based on the current strategy
        val interactions: List[Interaction] =
          for (client <- network.agents
               if randomDouble (0.0, 1.0) <= RequestProbability;
               service = chooseFrom (primaryCapabilities)) yield
            strategy.selectProvider (network, client, service, round).provideService (client, service, round)
        // Calculate the utility from the captured interactions
        val utility = interactions.map (calculateUtility).sum

        record (strategy, simulation, round, utility)

        // Notify agents of the end of the round
        for (agent <- network.agents)
          agent.endOfRound ()
      }

      // Perform all simulation rounds
      network.initialise ()
      for (round <- 0 until NumberOfRounds) {
        tick (round)
        print (".")
      }
      println ()
    }

    // Simulate the current network for each strategy
    for (strategy <- Strategies) {
      val start = System.currentTimeMillis
      println ("Simulating " + strategy + " attempt " + simulation)
      simulateStrategy (strategy)
      println ("Done after " + (System.currentTimeMillis - start) + "ms")
      network.clear ()
    }
  }

  // The utility gained from a single interaction (taking all terms to be of equal weight)
  def calculateUtility (interaction: Interaction) =
    average (interaction.ratings.values)

  // MAIN APPLICATION CODE BELOW

  val start = System.currentTimeMillis

  // Simulate the system for each selection strategy
  (0 until NumberOfSimulations).par.map (simulation => simulateStrategies (simulation))

  val format = new SimpleDateFormat ("MM-dd-HH-mm")
  val prefix = ResultsFile.takeWhile (_ != '.')
  val averagesFile = prefix + "-averages-" + format.format (new Date) + ".csv"
  val oneSimFile = prefix + "-onesim-" + format.format (new Date) + ".csv"

  println ("Recording results to " + ResultsFile)
  writeAll (cumulativeUtilities)
  println ("Writing cumulative averages to " + averagesFile)
  writeAverages (loadAll, averagesFile)
  println ("Writing one simulation utilities to " + oneSimFile)
  writeOneSimulation (oneSimUtilities, oneSimFile)

  println ("Duration: " + (System.currentTimeMillis - start))
}
