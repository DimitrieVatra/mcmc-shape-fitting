package fitting.parameters

import java.io.{File, PrintWriter}
import scalismo.geometry.{Point, _3D}
import scalismo.transformations.{RigidTransformation, Rotation3D, Translation3D, TranslationAfterRotation, TranslationAfterRotation3D}
import spray.json.{DeserializationException, JsNumber, JsObject, JsValue, JsonReader, JsonWriter, RootJsonFormat}

import scala.io.Source
import scala.util.Try

case class Sample(generatedBy: String, parameters: Parameters, rotationCenter: Point[_3D]) {
  def poseTransformation: TranslationAfterRotation[_3D] = {
    val translation = Translation3D(parameters.translationParameters)
    val rotation = Rotation3D(
      parameters.rotationParameters._1,
      parameters.rotationParameters._2,
      parameters.rotationParameters._3,
      rotationCenter
    )
    TranslationAfterRotation3D(translation, rotation)
  }
}

object Sample {

  implicit object Point3DJson extends RootJsonFormat[Point[_3D]] {

    override def read(value: JsValue): Point[_3D] = {
      value.asJsObject.getFields("x", "y", "z") match {
        case Seq(JsNumber(x), JsNumber(y), JsNumber(z)) => Point(x.toDouble, y.toDouble, z.toDouble)
      }
    }

    override def write(point: Point[_3D]): JsValue = {
      JsObject("x"->JsNumber(point.x), "y" -> JsNumber(point.y), "z" -> JsNumber(point.z))
    }
  }

  implicit object ParametersJSon extends RootJsonFormat[Sample] {
    override def read(value: JsValue): Sample = {
      value.asJsObject.getFields("rotationCenter", "parameters") match {
        case Seq(rotationCenter,parameters) => Sample(
          "loadedFromJSon",
          parameters.convertTo[Parameters],
          rotationCenter.convertTo[Point[_3D]]
        )
        case _ => throw new DeserializationException("Parameters not found")
      }
    }

    override def write(sample: Sample): JsValue = {
      import spray.json._
      JsObject(
        "rotationCenter"-> sample.rotationCenter.toJson,
        "parameters" -> sample.parameters.toJson,
      )
    }
  }

  def writeSample(sample : Sample, file : File)(implicit jsonWriter : JsonWriter[Sample])  : Try[Unit] = {
    Try {
      import spray.json._
      val jsonText = sample.toJson.prettyPrint
      val pw = new PrintWriter(file)
      pw.write(jsonText)
      pw.close
    }
  }

  def readSample(file : File)(implicit jsonReader : JsonReader[Sample]) : Try[Sample] = {
    Try {
      val text =  Source.fromFile(file).getLines().mkString("\n")

      import spray.json._
      val jsonAST = text.parseJson // or JsonParser(source)
      val sample = jsonAST.convertTo[Sample]
      sample
    }
  }
}