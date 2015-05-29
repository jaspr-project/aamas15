/**
 * (C) 2014 King's College London, University of Warwick
 */

import java.io.{PrintWriter, FileWriter, FileReader, BufferedReader}
import Configuration.{NumberOfSimulations, NumberOfRounds, ResultsFile, Strategies}
import Utilities.{average, createMap, createMutableMap, standardDeviation, toMap}

// Simulation results table
object Results {
  // Map of strategy name (String) to 2D array of simulation (first index) by round (second index), containing cumulative utility values
  val cumulativeUtilities = createMap (Strategies.map (_.name))(Array.fill (NumberOfSimulations, NumberOfRounds)(0.0))

  // Map of strategy name to array of per-round utilities, only recorded for a single simulation
  val oneSimUtilities = createMap (Strategies.map (_.name))(Array.fill (NumberOfRounds)(0.0))

  // Record a new result into the table
  def record (strategy: Strategy, simulation: Int, round: Int, result: Double) {
    // Record the per-round utility if on simulation 0
    if (simulation == 0)
      oneSimUtilities (strategy.name)(round) = result
    // Record the cumulative utility
    if (round == 0)
      cumulativeUtilities (strategy.name)(simulation)(round) = result
    else
      cumulativeUtilities (strategy.name)(simulation)(round) = cumulativeUtilities (strategy.name)(simulation)(round - 1) + result
  }
}

object ResultsAccess {
  // Load all the results contained in the configured results file
  def loadAll: Map[String, Array[Array[Double]]] = {
    val strategies = Strategies.map (_.name)
    val results = createMutableMap (strategies) (Array.fill (NumberOfRounds)(List[Double] ()))
    var numSims = 0
    val in = new BufferedReader (new FileReader (ResultsFile))

    var line = in.readLine
    while (line != null) {
      val parts = line.split (',')
      val strategy = parts (0)
      for (round <- 0 until NumberOfRounds)
        results (strategy)(round) = parts (round + 1).toDouble :: results (strategy)(round)
      if (strategy == strategies (0))
        numSims += 1
      line = in.readLine
    }
    in.close ()

    println ("Number of accumulated simulations: " + numSims)
    toMap (strategies) {
      strategy => Array.tabulate (numSims, NumberOfRounds) ((simulation, round) => results (strategy)(round)(simulation))
    }
  }

  // Write out all the results to the configured results file, with one row per simulation, one column per round
  def writeAll (utilities: Map[String, Array[Array[Double]]]) {
    val out = new PrintWriter (new FileWriter (ResultsFile, true))
    for (strategy <- utilities.keys; simulation <- 0 until NumberOfSimulations) {
      out.print (strategy)
      out.print (",")
      for (round <- 0 until NumberOfRounds) {
        out.print (utilities (strategy)(simulation)(round))
        out.print (",")
      }
      out.println ()
    }
    out.close ()
  }

  // Write the averages and standard deviations out to a file, with one row per round, two columns per strategy (average, std dev)
  def writeAverages (utilities: Map[String, Array[Array[Double]]], filename: String) {
    val out = new PrintWriter (new FileWriter (filename))
    val strategies = utilities.keys.toList
    out.print ("Round,")
    out.println (strategies.map (s => s + " avg," + s + " std dev").mkString (","))
    for (round <- 0 until NumberOfRounds) {
      out.print (round)
      for (strategy <- strategies) {
        val values = utilities (strategy).map (_ (round))
        val mean = average (values)
        val deviation = standardDeviation (values)
        out.print (",")
        out.print (mean)
        out.print (",")
        out.print (deviation)
      }
      out.println ()
    }
    out.close ()
  }

  // Write the utilities for a single simulation run to a file
  def writeOneSimulation (utilities: Map[String, Array[Double]], filename: String) {
    val out = new PrintWriter (new FileWriter (filename))
    for (strategy <- utilities.keys) {
      out.print (strategy)
      out.print (",")
      for (round <- 0 until NumberOfRounds) {
        out.print (utilities (strategy)(round))
        out.print (",")
      }
      out.println ()
    }
    out.close ()
  }
}
