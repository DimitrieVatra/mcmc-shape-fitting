package fitting.parameters

import breeze.linalg.DenseVector
import scalismo.geometry.{EuclideanVector, Point, _3D}
import spray.json.{JsArray, JsNumber, JsObject, JsValue, RootJsonFormat}
import spray.json.DefaultJsonProtocol._

case class Parameters(translationParameters: EuclideanVector[_3D],
                      rotationParameters: (Double, Double, Double),
                      modelCoefficients: DenseVector[Double],
                      landmarkNoiseStddev : Double)

object Parameters {

  implicit object EuclideanVectorJSon extends RootJsonFormat[EuclideanVector[_3D]] {

    override def read(value: JsValue): EuclideanVector[_3D] = {
      value.asJsObject.getFields("x", "y", "z") match {
        case Seq(JsNumber(x), JsNumber(y), JsNumber(z)) => EuclideanVector(x.toDouble, y.toDouble, z.toDouble)
      }
    }

    override def write(vec: EuclideanVector[_3D]): JsValue = {
      JsObject("x"->JsNumber(vec.x), "y" -> JsNumber(vec.y), "z" -> JsNumber(vec.z))
    }
  }

//  implicit object RotationParametersJSon extends RootJsonFormat[(Double, Double, Double)] {
//
//    override def read(value: JsValue): (Double, Double, Double) = {
//      value.asJsObject.getFields("phi", "psi", "rho") match {
//        case Seq(JsNumber(phi), JsNumber(psi), JsNumber(rho)) => (phi.toDouble, psi.toDouble, rho.toDouble)
//      }
//    }
//
//    override def write(rot: (Double, Double, Double)): JsValue = {
//      JsObject("phi"->JsNumber(rot._1), "psi" -> JsNumber(rot._2), "rho" -> JsNumber(rot._3))
//    }
//  }


  implicit object DenseVectorJSon extends RootJsonFormat[DenseVector[Double]] {
    override def read(value: JsValue): DenseVector[Double] = {
      value match {
        case JsArray(values) =>
          DenseVector[Double](values.toVector.map{
            case(JsNumber(v)) => v.toDouble
            case _ => throw new Exception("invalid number in vector ")
          }.toArray)
        case _ =>
          throw new Exception("expected an array but got " + value)
      }
    }

    override def write(vec: DenseVector[Double]): JsValue = {
      val valuesAsVector : Vector[JsValue] = vec.map(v => JsNumber(v)).toArray.toVector
      JsArray(valuesAsVector)
    }
  }



  implicit object ParametersJSon extends RootJsonFormat[Parameters] {

    override def read(value: JsValue): Parameters = {
      value.asJsObject.getFields("translationParameters", "rotationParameters", "modelCoefficients", "landmarkNoiseStddev") match {
        case Seq(transParams, rotParams, modelCoeffs, landmarkNoiseStddev) => Parameters(
          transParams.convertTo[EuclideanVector[_3D]],
          rotParams.convertTo[(Double, Double, Double)],
          modelCoeffs.convertTo[DenseVector[Double]],
          landmarkNoiseStddev.convertTo[Double]
        )
      }

    }

    override def write(params: Parameters): JsValue = {
      import spray.json._
      JsObject(
        "translationParameters"-> params.translationParameters.toJson,
        "rotationParameters" -> params.rotationParameters.toJson,
        "modelCoefficients" -> params.modelCoefficients.toJson,
        "landmarkNoiseStddev" -> params.landmarkNoiseStddev.toJson
      )
    }
  }
}
