package fitting.proposals

import fitting.parameters.Sample
import scalismo.sampling.{ProposalGenerator, TransitionProbability}

case class NoiseUpdateProposal(stddev: Double)(implicit rng : scalismo.utils.Random)
  extends ProposalGenerator[Sample]
    with TransitionProbability[Sample] {

  val perturbationDistr = breeze.stats.distributions.Gaussian(0, stddev)(rng.breezeRandBasis)

  def propose(sample: Sample): Sample = {
    val newSigma = sample.parameters.landmarkNoiseStddev +  perturbationDistr.sample()
    val newParameters = sample.parameters.copy(landmarkNoiseStddev = newSigma)
    sample.copy(generatedBy = s"NoiseStddevUpdateProposal ($stddev)", parameters = newParameters)
  }

  override def logTransitionProbability(from: Sample, to: Sample) = {
    val residual = to.parameters.landmarkNoiseStddev - from.parameters.landmarkNoiseStddev
    perturbationDistr.logPdf(residual)
  }
}