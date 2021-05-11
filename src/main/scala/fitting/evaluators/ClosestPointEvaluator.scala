package fitting.evaluators

import breeze.linalg.{DenseMatrix, DenseVector}
import fitting.parameters.Sample
import scalismo.common.interpolation.TriangleMeshInterpolator3D
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.sampling.DistributionEvaluator
import scalismo.statisticalmodel.{MultivariateNormalDistribution, PointDistributionModel}

case class ClosestPointEvaluator(model: PointDistributionModel[_3D, TriangleMesh],
                                 lmPoints: Seq[Point[_3D]])
  extends DistributionEvaluator[Sample] {



  val decimatedRef = model.reference.operations.decimate(targetedNumberOfVertices = 500)
  val decModel = model.newReference(decimatedRef, TriangleMeshInterpolator3D())

  override def logValue(sample: Sample): Double = {

    val lmUncertainty = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * sample.parameters.landmarkNoiseStddev)
    val instance = decModel.instance(sample.parameters.modelCoefficients).transform(sample.poseTransformation)
    val likelihoods = lmPoints.map(lmPoint => {
      val closestModelPoint = instance.operations.closestPointOnSurface(lmPoint).point
      val observedDeformation = lmPoint - closestModelPoint

      lmUncertainty.logpdf(observedDeformation.toBreezeVector)
    })

    val loglikelihood = likelihoods.sum
    loglikelihood
  }
}
