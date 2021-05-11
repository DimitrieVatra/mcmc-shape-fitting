package fitting.evaluators

import breeze.linalg.{DenseMatrix, DenseVector}
import fitting.evaluators.CorrespondenceEvaluator.marginalizeModelForCorrespondences
import scalismo.common.{PointId, UnstructuredPointsDomain}
import scalismo.geometry.{Point, _3D}
import scalismo.sampling.DistributionEvaluator
import scalismo.statisticalmodel.{MultivariateNormalDistribution, PointDistributionModel, StatisticalMeshModel}
import fitting.parameters.Sample
import scalismo.mesh.TriangleMesh

case class CorrespondenceEvaluator(model: PointDistributionModel[_3D, TriangleMesh],
                                   correspondences: Seq[(PointId, Point[_3D])])
  extends DistributionEvaluator[Sample] {

  val (marginalizedModel, newCorrespondences) = marginalizeModelForCorrespondences(model, correspondences)

  override def logValue(sample: Sample): Double = {

    val uncertainty = MultivariateNormalDistribution(DenseVector.zeros[Double](3),  DenseMatrix.eye[Double](3) * sample.parameters.landmarkNoiseStddev)
    val currModelInstance = marginalizedModel
      .instance(sample.parameters.modelCoefficients)
      .transform(sample.poseTransformation)

    val likelihoods = newCorrespondences.map(correspondence => {
      val (id, targetPoint) = correspondence
      val modelInstancePoint = currModelInstance.pointSet.point(id)
      val observedDeformation = targetPoint - modelInstancePoint

      uncertainty.logpdf(observedDeformation.toBreezeVector)
    })

    val loglikelihood = likelihoods.sum
    loglikelihood
  }
}

object CorrespondenceEvaluator {

  def marginalizeModelForCorrespondences(model: PointDistributionModel[_3D, TriangleMesh],
                                         correspondences: Seq[(PointId, Point[_3D])])
  : (PointDistributionModel[_3D, UnstructuredPointsDomain],
    Seq[(PointId, Point[_3D])]) = {

    val (modelIds, _) = correspondences.unzip
    val marginalizedModel = model.marginal(modelIds.toIndexedSeq)
    val newCorrespondences = correspondences.map(idWithTargetPoint => {
      val (id, targetPoint) = idWithTargetPoint
      val modelPoint = model.reference.pointSet.point(id)
      val newId = marginalizedModel.reference.pointSet.findClosestPoint(modelPoint).id
      (newId, targetPoint)
    })
    (marginalizedModel, newCorrespondences)
  }

}
