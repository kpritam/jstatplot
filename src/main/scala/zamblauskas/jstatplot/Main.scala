package zamblauskas.jstatplot

import java.io.{BufferedReader, File, InputStreamReader, PrintWriter}

import org.sameersingh.scalaplot.Chart
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter
import scalaz.EitherT.eitherT
import scalaz.Scalaz._
import scalaz._
import scalaz.effect._
import zamblauskas.csv.parser.{ColumnReads, Parser}
import zamblauskas.jstatplot.Graph.{Data, GraphSize, YRange}

import scala.collection.mutable.ArrayBuffer

object Main {

  def main(args: Array[String]) {
    Config.parser.parse(args, Config()).foreach { config =>
      val graphRange = config.graphRangeYTo.map(YRange(0, _))
      val graphSize = GraphSize(config.graphWidth, config.graphHeight)
      val titleSuffix = if (! config.titleSuffix.isEmpty) s" [${config.titleSuffix}]" else config.titleSuffix
      val results = config.jstatResults.map { file =>
        for {
          result <- createGraph(file, titleSuffix, config.skipNumLines, graphRange, graphSize, config.sizeUnit, config.multiplot)
          output <- result match {
            case -\/(err) => IO.putStrLn(err)
            case \/-(())  => IO.putStrLn(s"Plotted '$file'.")
          }
        } yield output
      }.sequenceU
      results.unsafePerformIO()
    }
  }

  private def createGraph(jstatResult: File, titleSuffix: String, skipNumLines: Int, yRange: Option[YRange], size: GraphSize, sizeUnit: SizeUnit, multiplot: Boolean) = {

    def plot[A](title: String, h: Seq[Data[A]])(implicit g: Graph[A]): IO[String \/ Unit] = {
      val chart = Graph.createChart(title, yRange, size, h)
      val name = jstatResult.getName + "-" + title.toLowerCase.replaceAll("\\s+", "-")
      val destination = jstatResult.getAbsoluteFile.getParentFile
      Graph.plot(chart, name, destination)
    }

    def mPlot[A](title: String, h: Seq[Data[A]])(implicit g: Graph[A]): IO[String \/ ArrayBuffer[String]] = {
      val chart = Graph.createChart(title, yRange, size, h)
      val name = jstatResult.getName + "-" + title.toLowerCase.replaceAll("\\s+", "-")
      val destination = jstatResult.getAbsoluteFile.getParentFile
      Graph.mPlot(chart, name, destination)
    }

    def parse[H <: Heap](csv: String)(implicit cr: ColumnReads[JStat[H]]): IO[String \/ Seq[JStat[H]]] = IO { \/.fromEither {
      Parser.parse[JStat[H]](csv)
        .leftMap(f => s"Parsing '${jstatResult.getAbsolutePath}' failed with '${f.message}' on line ${f.lineNum}.")
    }}

    implicit val heapJava7Graph: Graph[HeapJava7] = HeapJava7.heapGraph(sizeUnit.name, sizeUnit.extract)
    implicit val heapJava8Graph: Graph[HeapJava8] = HeapJava8.heapGraph(sizeUnit.name, sizeUnit.extract)

    def plotJstat[H <: Heap](csv: String)(implicit g: Graph[H], cr: ColumnReads[JStat[H]]): EitherT[IO, String, Unit] = {
      for {
        lines <- eitherT(parse(csv))
        _     <- eitherT(plot(s"Heap Capacity$titleSuffix", lines.map(j => Data(j.timestamp, j.capacity))))
        _     <- eitherT(plot(s"Heap Utilization$titleSuffix", lines.map(j => Data(j.timestamp, j.utilization))))
        _     <- eitherT(plot(s"Number of GC events$titleSuffix", lines.map(j => Data(j.timestamp, j.gcEvents))))
        _     <- eitherT(plot(s"GC time$titleSuffix", lines.map(j => Data(j.timestamp, j.gcTime))))
      } yield ()
    }

    def mPlotJstat[H <: Heap](csv: String)(implicit g: Graph[H], cr: ColumnReads[JStat[H]]): EitherT[IO, String, Unit] = {
      for {
        lines <- eitherT(parse(csv))
        hCapacity     <- eitherT(mPlot(s"Heap Capacity", lines.map(j => Data(j.timestamp, j.capacity))))
        hUtilization     <- eitherT(mPlot(s"Heap Utilization", lines.map(j => Data(j.timestamp, j.utilization))))
        gcEvents     <- eitherT(mPlot(s"Number of GC events", lines.map(j => Data(j.timestamp, j.gcEvents))))
        gcTime     <- eitherT(mPlot(s"GC time", lines.map(j => Data(j.timestamp, j.gcTime))))
      } yield  {

        val title = "GC Statistics"
        val mTitle = title.toLowerCase.replaceAll("\\s+", "-")
        val name = jstatResult.getName + "-" + mTitle
        val destination = jstatResult.getAbsoluteFile.getParentFile
        val directory = destination.getAbsolutePath + "/"
        val sizeString = "size %f,%f" format(size.x, size.y)
        val terminal = "pngcairo %s enhanced font 'Verdana,18'" format sizeString

        var lines = ArrayBuffer.empty[String]
        lines += "set terminal %s" format terminal
        lines += "set output \"%s\"" format (name + ".png")
        lines += "set xtics rotate"
        lines += s"set multiplot layout 2,2 rowsfirst title '$title'"
        lines ++= hCapacity.filterNot(_.contains("set terminal"))
        lines ++= hUtilization.filterNot(_.contains("set terminal"))
        lines ++= gcEvents.filterNot(_.contains("set terminal"))
        lines ++= gcTime.filterNot(_.contains("set terminal"))

        val scriptFile = directory + name + ".gpl"
        val writer = new PrintWriter(scriptFile)
        for (line <- lines) {
          writer.println(line)
        }
        writer.close()

        def runGnuplot(directory: String, filenamePrefix: String): String = {
          var line: String = ""
          var output = ""
          val cmdLine = "gnuplot " + filenamePrefix + ".gpl"

          try {
            val p = Runtime.getRuntime.exec(cmdLine, Array.empty[String], new File(directory))
            val input = new BufferedReader(new InputStreamReader(p.getInputStream()))
            while ( {
              line = input.readLine();
              line
            } != null) {
              output += (line + '\n')
            }
            input.close()
          }
          catch {
            case ex: Exception => ex.printStackTrace()
          }
          output
        }

        runGnuplot(directory, name)
      }
    }

    def error(msg: String): EitherT[IO, String, Unit] = eitherT(IO{msg.left[Unit]})

    (for {
      csv    <- eitherT(Reader.readAsCsvString(jstatResult, skipNumLines))
      _      <- if(Parser.isHeaderValid[JStat[HeapJava7]](csv))      if (multiplot) mPlotJstat[HeapJava7](csv) else plotJstat[HeapJava7](csv)
                else if(Parser.isHeaderValid[JStat[HeapJava8]](csv)) if (multiplot) mPlotJstat[HeapJava8](csv) else plotJstat[HeapJava8](csv)
                else                                                 error(s"Invalid result file ${jstatResult.getAbsolutePath}")
    } yield ()).run
  }
}
