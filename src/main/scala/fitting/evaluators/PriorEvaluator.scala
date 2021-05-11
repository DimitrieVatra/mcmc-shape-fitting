package fitting.evaluators

import scalismo.sampling.DistributionEvaluator
import scalismo.statisticalmodel.{PointDistributionModel, StatisticalMeshModel}
import fitting.parameters.Sample
import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh

case class PriorEvaluator(model: PointDistributionModel[_3D, TriangleMesh]) extends DistributionEvaluator[Sample] {

  val translationPrior = breeze.stats.distributions.Gaussian(0.0, 5.0)
  val rotationPrior = breeze.stats.distributions.Gaussian(0, 0.1)
  val noisePrior = breeze.stats.distributions.LogNormal(0.0, 0.25)

  override def logValue(sample: Sample): Double = {
    model.gp.logpdf(sample.parameters.modelCoefficients) +
      translationPrior.logPdf(sample.parameters.translationParameters.x) +
      translationPrior.logPdf(sample.parameters.translationParameters.y) +
      translationPrior.logPdf(sample.parameters.translationParameters.z) +
      rotationPrior.logPdf(sample.parameters.rotationParameters._1) +
      rotationPrior.logPdf(sample.parameters.rotationParameters._2) +
      rotationPrior.logPdf(sample.parameters.rotationParameters._3) +
      noisePrior.logPdf(sample.parameters.landmarkNoiseStddev)
  }
}