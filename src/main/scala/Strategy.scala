/**
 * (C) 2014 King's College London, University of Warwick
 */
import Chooser.chooseFrom

// An agent's strategy for choosing a provider for a service
abstract class Strategy {
  // The strategy's identifier for plots
  val name: String

  // Select the provider for a given service in a given round, or None if no agent provides that service or is selected
  def selectProvider (network: Network, client: Agent, service: Capability, round: Int): Agent

  override def toString = name
}

// Strategy where providers are selected at random with no accounting for history
object NoStrategy extends Strategy {
  val name = "Random"

  // Select a random capable provider
  def selectProvider (network: Network, client: Agent, service: Capability, round: Int): Agent =
    chooseFrom (network.capableOf (service))
}

/*
FIRE performs worse than our strategy when:
 i. Either strategy would have enough information to rank agents somewhat correctly
 ii. Agent A would be the best choice
 iii. There have been changes to A's circumstances over the past N rounds
 iv. There are not too many assessments of A accurate to the current circumstances
 v. The second choice agent is significantly worse than A
 */