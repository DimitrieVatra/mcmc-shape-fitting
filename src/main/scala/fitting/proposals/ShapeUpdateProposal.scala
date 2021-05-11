package fitting.proposals

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.sampling.{ProposalGenerator, TransitionProbability}
import scalismo.statisticalmodel.MultivariateNormalDistribution
import fitting.parameters.Sample


case class ShapeUpdateProposal(paramVectorSize : Int, stddev: Double)(implicit rng : scalismo.utils.Random)
  extends ProposalGenerator[Sample]  with TransitionProbability[Sample] {

  val perturbationDistr = new MultivariateNormalDistribution(
    DenseVector.zeros(paramVectorSize),
    DenseMatrix.eye[Double](paramVectorSize) * stddev * stddev
  )


  override def propose(sample: Sample): Sample = {
    val perturbation = perturbationDistr.sample()
    val newParameters = sample.parameters.copy(modelCoefficients = sample.parameters.modelCoefficients + perturbationDistr.sample)
    sample.copy(generatedBy = s"ShapeUpdateProposal ($stddev)", parameters = newParameters)
  }

  override def logTransitionProbability(from: Sample, to: Sample) = {
    val residual = to.parameters.modelCoefficients - from.parameters.modelCoefficients
    perturbationDistr.logpdf(residual)
  }
}
