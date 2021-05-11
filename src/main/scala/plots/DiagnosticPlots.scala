package plots

import com.cibo.evilplot.colors.RGB
import fitting.parameters.Sample

import java.io.File

object DiagnosticPlots {

  import com.cibo.evilplot.geometry.Extent
  import com.cibo.evilplot.numeric.Point
  import com.cibo.evilplot.plot._
  import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  import scalismo.sampling.DistributionEvaluator

  private val fontSize = 30

  def tracePlot(samples: Seq[Sample],
                evaluator: DistributionEvaluator[Sample],
                file : File): Unit = {

    val data = samples.zipWithIndex
      .map { case (sample, i) => Point(i.toDouble, evaluator.logValue(sample)) }
    val trace = LinePlot
      .series(data, "Line graph", RGB(1, 0, 0))
      .xAxis()
      .yAxis()
      .frame()
      .xLabel("Iteration", size = Some(fontSize))
      .title("Trace plot")
      .background(RGB(255, 255, 255))
    javax.imageio.ImageIO.write(
      trace.render(Extent(1400, 1400)).asBufferedImage,
      "png",
      file)

  }

  def histograms(samples: Seq[Sample], file : File,
            numBars: Int = 10) : Unit= {

    case class PlotDesc(extractor : Sample => Double, xlim : (Double, Double))
    val plotDesc = Seq(
        Map(
        "translation x" -> PlotDesc(((s : Sample) => s.parameters.translationParameters(0)), (-10.0, 10.0)),
        "rotation 1" -> PlotDesc((s : Sample) => s.parameters.rotationParameters._1, (-0.1, 0.1)),
        "noise" -> PlotDesc((s : Sample) => s.parameters.landmarkNoiseStddev, (0, 5))
        ),
        Map(
        "shapecoeff 1" -> PlotDesc((s : Sample) => s.parameters.modelCoefficients(0), (-3.0, 3.0)),
        "shapecoeff 2" -> PlotDesc((s : Sample) => s.parameters.modelCoefficients(1), (-3.0, 3.0)),
        "shapecoeff 3" -> PlotDesc((s : Sample) => s.parameters.modelCoefficients(2), (-3.0, 3.0)),
      )
    )

    val plots = for (parameterGroup <- plotDesc) yield {
      for ((title, desc) <- parameterGroup) yield {
          Histogram(samples.map(desc.extractor), binningFunction = Histogram.density, bins = numBars)
            .xAxis()
            .xbounds(desc.xlim._1, desc.xlim._2)
            .yAxis()
            .frame()
            .xLabel(title, size = Some(fontSize))
            .yLabel("Frequency", size = Some(fontSize))
      }
    }
    javax.imageio.ImageIO.write(
      Facets(
        plots.map(_.toSeq))
        .background(RGB(255,255,255)
        )
      .render(Extent(1400, 1400)).asBufferedImage,
        "png",
        file
    )

  }

  def histogram(samples: Seq[Double],
                 limits : (Double, Double), title : String, numBars: Int = 10,  file : File) : Unit= {

        val hist = Histogram(samples, binningFunction = Histogram.density, bins = numBars)
          .xAxis()
          .xbounds(limits._1, limits._2)
          .yAxis()
          .frame()
          .xLabel(title, size = Some(fontSize))
          .yLabel("Frequency", size = Some(fontSize))

    javax.imageio.ImageIO.write(

        hist.render(Extent(1400, 1400)).asBufferedImage,
      "png",
      file)
  }


  def pairs(samples: Seq[Sample], file : File) : Unit= {

    case class PlotDesc(name: String, extractor: Sample => Double, xLimit: (Double, Double))
    val plotDescs = Seq(
      //PlotDesc("noise (sigma)", (s: Sample) => s.parameters.noiseStddev, (0, 5.0)),
      PlotDesc("translation x", (s: Sample) => s.parameters.translationParameters.x, (-5, 5.0)),
      PlotDesc("rotation 1", (s: Sample) => s.parameters.rotationParameters._1, (-0.1, 0.1)),
      PlotDesc("shapecoeff 1", (s: Sample) => s.parameters.modelCoefficients(0), (-3.0, 3.0)),
      PlotDesc("shapecoeff 2", (s: Sample) => s.parameters.modelCoefficients(1), (-3.0, 3.0)),
      //PlotDesc("shapecoeff 3", (s: Sample) => s.parameters.modelCoefficients(2), (-3.0, 3.0)),
    )

    val plots = for ((plotDesc1, i) <- plotDescs.zipWithIndex) yield {
      for ((plotDesc2, j) <- plotDescs.zipWithIndex) yield {
        if (i > j) {
          val points = samples.map(plotDesc1.extractor).zip(samples.map(plotDesc2.extractor)).map { case (s1, s2) => Point(s1, s2) }
          ScatterPlot(points)
            .xAxis()
            .yAxis()
            .frame()
            .xLabel(plotDesc1.name, size = Some(fontSize))
            .yLabel(plotDesc2.name, size = Some(fontSize))
        } else if (i == j) {
          Histogram(samples.map(plotDesc1.extractor), binningFunction = Histogram.density, bins = 10)
            .xAxis()
            .xbounds(plotDesc1.xLimit._1, plotDesc1.xLimit._2)
            .yAxis()
            .frame()
            .xLabel(plotDesc1.name, size = Some(fontSize))
            .yLabel("Frequency",size = Some(fontSize))
        } else {
          val points = samples.map(plotDesc1.extractor).zip(samples.map(plotDesc2.extractor)).map { case (s1, s2) => Point(s1, s2) }
          ContourPlot(points)
            .xAxis()
            .yAxis()
            .frame()
            .xLabel(plotDesc2.name, size = Some(fontSize))
            .yLabel(plotDesc1.name, size = Some(fontSize))
        }
      }
    }
    javax.imageio.ImageIO.write(
      Facets(plots)
        .background(RGB(255, 255, 255)
        )
        .render(Extent(1400, 1400)).asBufferedImage,
      "png",
      file)
    }
  }

