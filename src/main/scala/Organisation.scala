/**
 * (C) 2014 King's College London, University of Warwick
 */
import Chooser.ifHappens
import Configuration.{CultureEffects, ProbabilityOfBadCulture}
import Utilities.createMap

// An organisation that an agent can be working for
class Organisation (network: Network) {
  import network._

  // Effect of organisation on service provision depending on whether the culture is bad or good
  val culture = ifHappens (ProbabilityOfBadCulture) (CultureEffects._2) (CultureEffects._1)
  // The organisation's competence at supporting agents in providing each of its capabilities, with regard to each term (provision feature)
  val competence: Map[Capability, Map[Term, Double]] = createMap (primaryCapabilities) (createMap (terms) (culture))
}
