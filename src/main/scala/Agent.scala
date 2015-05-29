/**
 * (C) 2014 King's College London, University of Warwick
 */

import Chooser.{chooseFrom, ifHappens, randomDouble, randomInt, randomSubset}
import Configuration._
import Math.sqrt
import Utilities.{createMap, toMap}
import scala.annotation.tailrec

// Static methods to help construct an agent
object Agent {
  // Calculates the distance between two agents in the spherical grid world
  def distance (agent1: Agent, agent2: Agent): Double = {
    // The minimum distance along one dimension given two coordinates
    def minDistance (p1: Int, p2: Int) =
      (p1 - p2).abs.min (((p1 + GridWidth) - p2).abs).min ((p1 - (p2 + GridWidth)).abs)

    val dx = minDistance (agent1.position._1, agent2.position._1).toDouble
    val dy = minDistance (agent1.position._2, agent2.position._2).toDouble
    sqrt (dx * dx + dy * dy)
  }

  // Retrieves all direct and indirect acquaintances of a given agent, including the start agent itself
  def gatherAcquaintances (startAgent: Agent): Set[Agent] = {
    // Recursively retrieve the acquaintances of the agents in toAsk, adding to known
    @tailrec
    def gatherAcquaintances (known: Set[Agent], toAsk: Set[Agent]): Set[Agent] =
      if (toAsk.isEmpty)
        known
      else {
        val list = toAsk.toList   // To ensure head and tail provide consistent exclusive values
        gatherAcquaintances (known + list.head, list.head.neighbours.filter (n => !known.contains (n)) ++ list.tail)
      }

    gatherAcquaintances (Set (), Set (startAgent))
  }
}
import Agent._

// An agent in the network
class Agent (network: Network) {
  import network._

  // The agent's (x, y) position in the neighbourhood-determining grid
  val position = (randomInt (0, GridWidth), randomInt (0, GridWidth))

  // The agent's neighbours in the network (lazily loaded to ensure all agents in network created before neighbours chosen)
  lazy val neighbours: Set[Agent] = agents.filter (other => other != this && distance (this, other) <= NeighbourRadius).toSet

  // All direct and indirect acquaintances of this agent, including this agent itself
  lazy val acquaintances: Set[Agent] = gatherAcquaintances (this)

  // The agent's set of capabilities
  val capabilities: List[Capability] = randomSubset (allCapabilities, NumberOfCapabilitiesPerAgent)

  // Whether this agent is a good service provider
  val goodProvider = randomDouble (0.0, 1.0) < GoodProviderProbability

  // The agent's competence at performing each of its capabilities, with regard to each term (provision feature)
  val competence: Map[Capability, Map[Term, Double]] =
    if (goodProvider)
      createMap (capabilities) (createMap (terms) (chooseFrom (PossibleGoodCompetencies)))
    else
      createMap (capabilities) (createMap (terms) (chooseFrom (PossibleBadCompetencies)))

  // The secondary capabilities for which the agent must depend on a subprovider
  val dependentFor: List[Capability] = capabilities.flatMap (_.dependsOn).filter (!capabilities.contains (_))

  // Rounds until the next re-selection of sub-providers (first set on initialisation)
  var periodToSwitchSubproviders: Int = 0

  // Sub-providers currently chosen to provide capabilities the agent depends on others for (first set on initialisation)
  var subproviders: Map[Capability, Agent] = Map ()

  // Rounds until the next re-selection of owner organisation (first set on initialisation)
  var periodToSwitchOrganisation: Int = 0

  // Organisation currently employing this agent
  var organisation: Organisation = null

  // History of interactions this agent has been engaged in as a client
  var provenanceStore: List[Interaction] = Nil
  //val provenanceStore = scala.collection.mutable.Map[(Agent, Capability), List[Interaction]] ()

  // Initialise the agent ready for simulation
  def initialise () {
    reselect ()
  }

  // Select new sub-providers and choose a new period before the next re-selection
  def reselect () {
    subproviders = toMap (dependentFor) (capability => chooseFrom (capableOf (capability)))
    periodToSwitchSubproviders = randomInt (MinimumSwitchPeriod, MaximumSwitchPeriod + 1)
    organisation = chooseFrom (organisations)
    periodToSwitchOrganisation = randomInt (MinimumSwitchPeriod, MaximumSwitchPeriod + 1)
  }

  // Records the completion of a round, triggering re-selection of sub-providers if appropriate
  def endOfRound () {
    if (periodToSwitchSubproviders <= 1) reselect () else periodToSwitchSubproviders -= 1
  }

  // Request that this agent perform a service interacting with a client, storing and returning the provenance record documenting what happened
  def provideService (client: Agent, service: Capability, round: Int): Interaction = {
    val orgCompetence = organisation.competence (service)
    // Combine the competence of one agent with that of another to get overall ratings
    def affected (a: Map[Term, Double], b: Map[Term, Double]): Map[Term, Double] =
      a.map {case (term, abilityA) => (term, (abilityA + b (term)) / 2.0)}
    // Create an interaction where no sub-provider is required
    def provideServiceWithoutDependence: Interaction =
      ifHappens (FreakEventProbability) (provideServiceWithFreakEvent) (provideBasicService)
    // Create an interaction where no mitigating circumstances occur
    def provideBasicService: Interaction =
      if (OrganisationsMatter)
        BasicProvision (client, this, service, organisation, round, affected (competence (service), orgCompetence))
      else
        BasicProvision (client, this, service, organisation, round, competence (service))
    // Create an interaction where a freak event affects provision
    def provideServiceWithFreakEvent: Interaction = {
      val prior =
        if (OrganisationsMatter)
          affected (competence (service), orgCompetence)
        else
          competence (service)
      FreakEvent (client, this, service, organisation, round, affected (prior, freakEventEffects), prior, "storm")
    }
    // Create an interaction where provision relies on a sub-provider
    def provideServiceWithDependence (subservice: Capability): Interaction = {
      val subprovider = subproviders (subservice)
      val subcompetence = subprovider.competence (subservice)
      val finalCompetence =
        if (OrganisationsMatter)
          affected (affected (competence (service), orgCompetence), subcompetence)
        else
          affected (competence (service), subcompetence)
      WithSubProvider (client, this, service, organisation, round, finalCompetence, subprovider, subservice, subcompetence)
    }

    val interaction = service.dependsOn match {
      // The service does not need a sub-service to provide
      case None => provideServiceWithoutDependence
      // The service does need a sub-service to provide, but this agent can also provide the sub-service
      case Some (subservice) if !dependentFor.contains (subservice) => provideServiceWithoutDependence
      // The service does need a sub-service to provide, and this agent must rely on a sub-provider
      case Some (subservice) if dependentFor.contains (subservice) => provideServiceWithDependence (subservice)
    }
    client.recordProvenance (interaction)
    interaction
  }

  // Record an interaction to the agent's provenance store (should be one in which this agent was the client)
  def recordProvenance (interaction: Interaction) {
    provenanceStore = (interaction :: provenanceStore).take (MemoryLimit)
  }

  // Retrieve the provenance records related to a given provider and a given service type from the agent's own store and that of its acquaintances
  def gatherProvenance (provider: Agent, service: Capability): Set[Interaction] =
    acquaintances.flatMap (_.provenanceStore.filter (interaction => interaction.provider == provider && interaction.service == service))

  def clear () {
    provenanceStore = Nil
  }
}
