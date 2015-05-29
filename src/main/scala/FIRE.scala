/**
 * (C) 2014 King's College London, University of Warwick
 */

import java.lang.Math.{E, pow}
import Chooser.ifHappens
import Configuration.{ExplorationProbability, RecencyScalingFactor}
import Utilities.average

// The core FIRE strategy but leaving unspecified how to calculate trust from gathered interactions
trait FIRECore extends Strategy {
  // Select the provider mostly using FIRE's model
  def selectProvider (network: Network, client: Agent, service: Capability, round: Int): Agent = {
    // Choose first agent in list, except if exploration probability met try a later agent recursively
    def selectMostTrustworthy (orderedProviders: List[Agent]): Option[Agent] = orderedProviders match {
      case Nil => None
      case mostTrusted :: rest => ifHappens (ExplorationProbability) (selectMostTrustworthy (rest)) (Some (mostTrusted))
    }

    // Order the potential providers by their overall trust value
    val orderedProviders = network.capableOf (service).sortBy (calculateTrust (network, client, _, service, round)).reverse
    // Explore the set of providers, but if none chosen in exploration, choose the most trustworthy
    selectMostTrustworthy (orderedProviders).getOrElse (orderedProviders.head)
  }

  // Calculates the trust in a provider for a service
  def calculateTrust (network: Network, client: Agent, provider: Agent, service: Capability, round: Int): Double = {
    // Gather all relevant interaction records
    val interactions = client.gatherProvenance (provider, service)
    // Per term, calculate the weighted trust value based on the interactions
    def calculateTermTrust (term: Term): Double = {
      val weightsAndRatings = interactions.map (interaction => (calculateRelevance (interaction, term, round), interaction.ratings (term)))
      val weightedRatings = weightsAndRatings.map (x => x._1 * x._2).sum
      val weightsSum = weightsAndRatings.map (_._1).sum
      weightedRatings / weightsSum
    }
    // Average over the term-specific trust values
    average (network.terms.map (calculateTermTrust))
  }

  def calculateRecency (interaction: Interaction, round: Int): Double =
    pow (E, -((round - interaction.round) / RecencyScalingFactor))

  // Calculates the relevance of an interaction: omega_K(r_i)
  def calculateRelevance (interaction: Interaction, term: Term, round: Int): Double
}

// FIRE strategy implementation using recency to weight ratings
object FIRE extends Strategy with FIRECore {
  val name = "FIRE"
  def calculateRelevance (interaction: Interaction, term: Term, round: Int): Double = calculateRecency (interaction, round)
}

// Variation on FIRE where recency is not accounted for
object FIREWithoutRecency extends Strategy with FIRECore {
  val name = "FIREWithoutRecency"
  def calculateRelevance (interaction: Interaction, term: Term, round: Int): Double = 1.0
}
