/**
 * (C) 2014 King's College London, University of Warwick
 */

// A (summarised, OOP version of a) provenance record concerning an interaction, specialised by different scenarios
sealed abstract class Interaction {
  val client: Agent
  val provider: Agent
  val service: Capability
  val organisation: Organisation
  val round: Int
  val ratings: Map[Term, Double]
}

// Scenario where client requests service from provider who does not depend on any other agent
case class BasicProvision (client: Agent, provider: Agent, service: Capability, organisation: Organisation, round: Int,
                           ratings: Map[Term, Double]) extends Interaction

// Scenario where client requests service from provider who then uses a sub-provider
case class WithSubProvider (client: Agent, provider: Agent, service: Capability, organisation: Organisation, round: Int,
                            ratings: Map[Term, Double], subprovider: Agent, subservice: Capability, subratings: Map[Term, Double]) extends Interaction

// Scenario where client requests service from a provider, but provision is affected by a freak event
case class FreakEvent (client: Agent, provider: Agent, service: Capability, organisation: Organisation, round: Int,
                       ratings: Map[Term, Double], beforeEvent: Map[Term, Double], event: String) extends Interaction
