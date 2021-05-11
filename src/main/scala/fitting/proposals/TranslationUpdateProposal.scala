package fitting.proposals

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.geometry.EuclideanVector
import scalismo.sampling.{ProposalGenerator, TransitionProbability}
import scalismo.statisticalmodel.MultivariateNormalDistribution
import fitting.parameters.Sample

case class TranslationUpdateProposal(stddev: Double)(implicit rng : scalismo.utils.Random) extends
  ProposalGenerator[Sample]  with TransitionProbability[Sample] {

  val perturbationDistr = new MultivariateNormalDistribution( DenseVector.zeros(3),
    DenseMatrix.eye[Double](3) * stddev * stddev)

  def propose(sample: Sample): Sample= {
    val newTranslationParameters = sample.parameters.translationParameters + EuclideanVector.fromBreezeVector(perturbationDistr.sample())
    val newParameters = sample.parameters.copy(translationParameters = newTranslationParameters)
    sample.copy(generatedBy = s"TranlationUpdateProposal ($stddev)", parameters = newParameters)
  }

  override def logTransitionProbability(from: Sample, to: Sample) = {
    val residual = to.parameters.translationParameters - from.parameters.translationParameters
    perturbationDistr.logpdf(residual.toBreezeVector)
  }
}

