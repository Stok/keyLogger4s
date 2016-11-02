package keytypeAnalysis

import scala.swing._
import scala.swing.event._
import java.awt.Dimension
import java.io.{BufferedWriter, File, FileWriter}

class inputUI extends MainFrame {
  title = "Keyboard type pattern analysis"
  preferredSize = new Dimension(1240, 250)
  var inputTextField = new TextArea { rows = 8; lineWrap = true; wordWrap = true }
  var rawEventLogText = ""
  var typeID = 0
  var tBetweenKeys = Seq(("","",0.0))
  var k2kTimes = Seq(("", "", 0.0, 0.0))
  var output = ""
  var savedInputWordsForNextUse: List[keyPressSequence] = List()
  contents = new BoxPanel(Orientation.Vertical) {
    contents += Swing.VStrut(5)
    contents += new Label("Type \"This is a test\" in the field below:")
    contents += Swing.VStrut(10)
    contents += Swing.Glue
    contents += inputTextField
    contents += new ScrollPane(inputTextField)
    contents += Swing.VStrut(10)
  }
  menuBar = new MenuBar {

    contents += new Menu ("File") {
      contents += new MenuItem(Action("Clear textbox") {
        rawEventLogText = ""
        inputTextField.text = ""
      })
      contents += new MenuItem(Action("Load previously saved keyPresses") {
        readDataFromFile
      })
      contents += new MenuItem(Action("Save all input to .csv file") {
        saveAnalysedData(savedInputWordsForNextUse)
      })
      contents += new MenuItem(Action("Quit") {sys.exit(0)})
    }

    contents += new Menu("Analysis") {
      contents += new MenuItem(Action("Analyse and prepare next input") {
        val newInput = parseNewInput(rawEventLogText)
        if(newInput.keyPresses.nonEmpty) {
          savedInputWordsForNextUse = savedInputWordsForNextUse :+ newInput
        }
        rawEventLogText = ""
        inputTextField.text = ""
      })
      contents += new MenuItem(Action("Analyse and compare with previous input") {
        if (savedInputWordsForNextUse.nonEmpty) {
          val newInput:keyPressSequence = parseNewInput(rawEventLogText)
          val inputComparisons: Seq[Double] = savedInputWordsForNextUse.flatMap(_.compare(newInput))
          inputComparisons.zipWithIndex.foreach(x => println("Difference between new and previous input # " + x._2 + ": " + x._1))
          val mostRelatedPreviousInput = inputComparisons.indexOf(inputComparisons.min)
          println("I think this new data most closely resembles input # " + mostRelatedPreviousInput + ".")
        }
      })
      contents += new MenuItem(Action("Clear all recorded patterns") {
        rawEventLogText = ""
        inputTextField.text = ""
        savedInputWordsForNextUse = List()
      })
    }
  }
  listenTo(inputTextField.keys)
  reactions += {
    case KeyPressed(_, keyValue, _, _) =>
      val keyPressTime = System.nanoTime
      rawEventLogText += "p " + keyValue + " " + keyPressTime.toString + "\n"

    case KeyReleased(x,keyValue,y,z) =>
      val keyReleaseTime = System.nanoTime
      rawEventLogText += "r " + keyValue + " " + keyReleaseTime.toString + "\n"
  }

  def parseNewInput(rawText: String) : keyPressSequence = {
    if(rawEventLogText.length > 0) {
      eventParser.parseText(rawText)
    }
    else {
      println("No input found. Type some text before starting analysis")
      keyPressSequence(Seq())
    }
  }

  def saveAnalysedData(savedKPSeqs: Seq[keyPressSequence]) : Unit = {
    FileIOHelper.WriteToFile(savedKPSeqs)
  }

  def readDataFromFile = {
    savedInputWordsForNextUse = FileIOHelper.ReadFromFile match {
      case Some(f) => f.toList
      case None => List()
    }
  }


}
