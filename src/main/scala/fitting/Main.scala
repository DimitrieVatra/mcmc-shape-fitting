package fitting

import breeze.linalg.DenseVector
import fitting.evaluators.{ASMEvaluator, ClosestPointEvaluator, CorrespondenceEvaluator, PriorEvaluator}
import fitting.logger.Logger
import fitting.parameters.{Parameters, Sample}
import fitting.proposals.{NoiseUpdateProposal, RotationUpdateProposal, ShapeUpdateProposal, TranslationUpdateProposal}
import plots.DiagnosticPlots
import scalismo.common.PointId
import scalismo.common.interpolation.BSplineImageInterpolator3D
import scalismo.geometry.{EuclideanVector3D, Point, _3D}
import scalismo.image.DiscreteImage
import scalismo.io.{ActiveShapeModelIO, ImageIO, LandmarkIO}
import scalismo.mesh.TriangleMesh
import scalismo.sampling.algorithms.MetropolisHastings
import scalismo.sampling.evaluators.ProductEvaluator
import scalismo.sampling.proposals.MixtureProposal
import scalismo.statisticalmodel.{PointDistributionModel, StatisticalMeshModel}
import scalismo.statisticalmodel.asm.ActiveShapeModel
import scalismo.ui.api.ScalismoUI

import java.awt.Color
import java.io.File

object Main {

  // This helper method is a hack to work around a bug in Scalismo, namely that the ASM class returns
  // a StatisticalMeshModel instead of a PointDistribution model. This method automagically does the
  // conversion.
  implicit def ssmToPDM(meshModel : StatisticalMeshModel) : PointDistributionModel[_3D, TriangleMesh] = {
    PointDistributionModel(meshModel.gp)
  }
//
  // Setting up the ui
  val ui = ScalismoUI()
  val modelGroup = ui.createGroup("model")
  val imgGroup = ui.createGroup("image")


  def fitModel(asm: ActiveShapeModel,
              correspondencePoints: Seq[(PointId, Point[_3D])],
              contourPoints: Seq[Point[_3D]],
              image: DiscreteImage[_3D, Short])
             (implicit rng: scalismo.utils.Random): Unit = {

    val shapeModel = asm.statisticalModel

    val modelView = ui.show(modelGroup, shapeModel, "volume model")
    val gpTrans = modelView.shapeModelTransformationView.shapeTransformationView
    val poseTrans = modelView.shapeModelTransformationView.poseTransformationView

    val closestPointEvaluator = ClosestPointEvaluator(shapeModel, contourPoints)
    val correspondenceEvaluator = CorrespondenceEvaluator(shapeModel, correspondences = correspondencePoints)
    val priorEvaluator = PriorEvaluator(shapeModel)
    val asmEvaluator = ASMEvaluator(asm, asm.preprocessor.apply(image.map(_.toFloat)))
    val posteriorEvaluator = ProductEvaluator(closestPointEvaluator, correspondenceEvaluator, priorEvaluator, asmEvaluator)


    val rotationUpdateProposal = RotationUpdateProposal(1.0)
    val translationUpdateProposal = TranslationUpdateProposal(1.0)
    val shapeUpdateProposal = ShapeUpdateProposal(shapeModel.rank, 1.0)
    val noiseUpdateProposal = NoiseUpdateProposal(1.0)

    val generator = MixtureProposal.fromProposalsWithTransition(
      (0.1, rotationUpdateProposal),
      (0.1, translationUpdateProposal),
      (0.1, noiseUpdateProposal),
      (0.7, shapeUpdateProposal)
    )

    val logger = Logger()
    val mh = MetropolisHastings(generator, posteriorEvaluator)

    // the rotation center is the center of mass of the reference mesh
    val rotationCenter = {
      val normFactor = 1.0 / shapeModel.referenceMesh.pointSet.numberOfPoints
      shapeModel.referenceMesh.pointSet.points.foldLeft(Point(0, 0, 0))((sum, point) => sum + point.toVector * normFactor)
    }


    val initialSample = Sample(
      "initialSample",
      Parameters(
        translationParameters = EuclideanVector3D(0.0, 0.0, 0.0),
        rotationParameters = (0.0, 0.0, 0.0),
        modelCoefficients = DenseVector.zeros[Double](shapeModel.rank),
        1.0
      ),
      rotationCenter
    )
    // alternatively, we could read a previously stored sample
    //val initialSample = Sample.readSample(new File("storedParameters.json")).get


    val samplingIterator = mh.iterator(initialSample, logger)

    // We augment the iterator such that in each n-th iteration the current shape is shown
    val iteratorWithVisualization = for ((sample, itNumber) <- samplingIterator.zipWithIndex) yield {
      println(s"in iteration $itNumber" )
      if (itNumber % 100 == 0) {
        modelView.shapeModelTransformationView.shapeTransformationView.coefficients = sample.parameters.modelCoefficients
        modelView.shapeModelTransformationView.poseTransformationView.transformation = sample.poseTransformation
      }
      sample
    }

    // we generate some samples
    val samples = iteratorWithVisualization.drop(1000).take(1000).toIndexedSeq

    // some diagnostic plots
    DiagnosticPlots.tracePlot(samples, posteriorEvaluator, new File("plots/traceplot.png"))
    DiagnosticPlots.pairs(samples, new File("plots/pairs.png") )

    // We can store the parameters to use it later
    // Sample.writeSample(samples.last, new File("storedParameters.json")).get

    println("acceptance ration : \n" +logger.acceptanceRatios())

  }


  def main(args: Array[String]): Unit = {
    scalismo.initialize()
    implicit val rng = scalismo.utils.Random(42)

    val activeShapeModel = ActiveShapeModelIO.readActiveShapeModel(new java.io.File("./datasets/asm/liver-asm.h5")).get
    val shapeModel = activeShapeModel.statisticalModel

    val modelLandmarks = LandmarkIO.readLandmarksJson[_3D](new java.io.File("datasets/asm/liver-asm-lm.json")).get

    val targetLandmarks = LandmarkIO.readLandmarksJson[_3D](new java.io.File("datasets/validationset/landmarks/liver-orig011.json")).get
    val correspondencePoints = modelLandmarks
      .map(lm => lm.point) // extract point from landmark
      .map(point => shapeModel.referenceMesh.pointSet.findClosestPoint(point).id) // find point id
      .zip(targetLandmarks.map(lm => lm.point)) // pair it with targetLandmarkPoint

    val contourLandmarks = LandmarkIO.readLandmarksJson[_3D](
      new java.io.File("./datasets/validationset/annotations/liver-011.json")
    ).get
    val contourPoints = contourLandmarks.map(lm => lm.point) //

    val discreteTargetImage = ImageIO.read3DScalarImageAsType[Short](new java.io.File("datasets/validationset/volume-ct/liver-011.nii")).get

    // visualizing all the data

    val targetImage = discreteTargetImage.interpolate(BSplineImageInterpolator3D[Short](3))
    val imgView = ui.show(imgGroup, discreteTargetImage, "image")

    val modelLmViews = ui.show(modelGroup, modelLandmarks, "model landmarks")
    modelLmViews.map(lmView => lmView.color = Color.RED)

    val contourLandmarkViews = ui.show(imgGroup, contourLandmarks, "contourPoints")
    contourLandmarkViews.map(cLmView => cLmView.color = Color.BLUE)

    val targetLandmarkViews = ui.show(imgGroup, targetLandmarks, "targetLandmarks")
    val targetLmView = ui.show(imgGroup, targetLandmarks, "target landmarks")
    targetLmView.map(lmView => lmView.color = Color.GREEN)

    // Starting the fittign process
    fitModel(activeShapeModel, correspondencePoints, contourPoints, discreteTargetImage)

  }
}
