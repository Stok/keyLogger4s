package keytypeAnalysis

import java.io._
import scala.swing._

import org.joda._
import org.joda.time.DateTime

/**
  *
  */
object FileIOHelper {

  def WriteToFile(KPSeqs: Seq[keyPressSequence]) = {
    val csvData = KPSeqs.map(_.toCSV).mkString("","\nNewUser\n","")

    val file = getSaveFileSelection match {
      case Some(f) => {
        val nowString = DateTime.now().toString("yyyy-MM-dd_HH_mm_ss")
        val outputFile = new File(f.getAbsoluteFile + "_"+ nowString + ".ks.csv")
        val bw = new BufferedWriter(new FileWriter(outputFile))
        bw.write(csvData)
        bw.close()
      }
      case None => ;
    }


  }

  def ReadFromFile : Option[Seq[keyPressSequence]] ={
    getOpenFileSelection match {
      case Some(file) => {
        val source = scala.io.Source.fromFile(file)(scala.io.Codec.UTF8)
        val csvData = try source.mkString.split("\nNewUser\n").map(_.split("\n").map(_.split(",").map(_.trim))) finally source.close()
        Some(csvData.map(KPSeq => keyPressSequence(KPSeq.map(kp => keyPress(kp(1).toDouble, kp(2).toDouble, kp(0))))).toSeq)
      }
    }

  }

  def getOpenFileSelection: Option[File] =
  {
    val chooser = new FileChooser(new File(".ks.csv"))
    chooser.title = "Choose keyStrokeData file"
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) {
      println("Approve -- " + chooser.selectedFile)
      Some(chooser.selectedFile)
    } else None
  }

  def getSaveFileSelection: Option[File] =
  {
    val chooser = new FileChooser(new File(".ks.csv"))
    chooser.title = "Choose keyStrokeData file"
    val result = chooser.showSaveDialog(null)
    if (result == FileChooser.Result.Approve) {
      println("Approve -- " + chooser.selectedFile)
      Some(chooser.selectedFile)
    } else None
  }

}

