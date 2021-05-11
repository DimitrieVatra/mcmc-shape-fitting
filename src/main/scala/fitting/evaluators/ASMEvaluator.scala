package fitting.evaluators

import fitting.parameters.{Parameters, Sample}
import scalismo.sampling.DistributionEvaluator
import scalismo.statisticalmodel.asm.{ActiveShapeModel, PreprocessedImage}


case class ASMEvaluator(asm: ActiveShapeModel, target: PreprocessedImage) extends DistributionEvaluator[Sample] {

    val referenceMesh = asm.statisticalModel.referenceMesh

    override def logValue(sample: Sample): Double = {

      val currentInstance = asm.statisticalModel
        .instance(sample.parameters.modelCoefficients)
        .transform(sample.poseTransformation)

      val distances = for {
        profileID <- asm.profiles.ids
        profile = asm.profiles(profileID)
        profilePointOnMesh = currentInstance.pointSet.point(profile.pointId)
        featureOpt = asm.featureExtractor(target, profilePointOnMesh, currentInstance, profile.pointId)
        if (featureOpt.isDefined)
      } yield {

        val featureAtPoint = featureOpt.get
        profile.distribution.logpdf(featureAtPoint)
      }

      distances.sum
    }


}


