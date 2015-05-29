/**
 * (C) 2014 King's College London, University of Warwick
 */

import Chooser.{getIfHappens, randomDouble}
import Configuration._
import Utilities.{createMap, toMap}

// A set of agents, each connected to a set of neighbours
class Network private () {
  // Terms (features) of services provided in the network
  val terms = List.fill (NumberOfTerms) (new Term)

  // List of primary capabilities, each optionally depending on a secondary capability
  val primaryCapabilities = List.fill (NumberOfPrimaryCapabilities) {
    new Capability (getIfHappens (ProbabilityOfDependence) (new Capability (None)))
  }

  // List of secondary capabilities
  val secondaryCapabilities = primaryCapabilities.flatMap (_.dependsOn)

  // List of all capabilities
  val allCapabilities = primaryCapabilities ::: secondaryCapabilities

  // The set of organisations in the network
  val organisations = List.fill (NumberOfOrganisations) (new Organisation (this))

  // The set of agents in the network
  val agents = List.fill (NumberOfAgents) (new Agent (this))

  // The effect of a freak event on the timeliness of provision in this network
  val freakEventEffects = createMap (terms) (1.0) //randomDouble (-1.0, -0.5))

  // A map from each capability to the agents with that capability
  val capableOf: Map[Capability, List[Agent]] = toMap (allCapabilities) { capability =>
    agents.filter (_.capabilities.contains (capability))
  }

  // Checks that this network contains at least one agent with each capability
  def checkFeasible: Boolean =
    allCapabilities.map (capableOf (_).size > 0).reduce (_ && _)

  // Initialise all agents in the network
  def initialise () {
    agents.foreach (_.initialise ())
  }

  def clear () {
    agents.foreach (_.clear ())
  }
}

object Network {
  // Creates a network of agents which has been checked to contain at least one agent with each capability
  def apply (): Network = {
    val network = new Network
    if (network.checkFeasible)
      network
    else
      apply ()
  }
}
