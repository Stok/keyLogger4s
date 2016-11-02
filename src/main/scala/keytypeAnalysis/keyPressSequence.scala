package keytypeAnalysis

/**
  *
  */
case class keyPressSequence(kPs: Seq[keyPress]) {



  val keyPresses: Seq[keyPress] = kPs//removeBackspaces(kPs) Should implement this at some point.

  val rawInputString: String = keyPresses.map{ x => x.letter}.mkString
  val lettersOnlyString: String = keyPresses.filterNot(_.letter == "⇧").map{x => x.letter}.mkString
  val pressDurations: Seq[(String, Double)] = keyPresses.map(x => (x.letter, x.keyPressDuration))
  val averageKeyPressDuration: (Double, Double) = {
    (stats.mean(pressDurations.map(_._2)), stats.stdErrOnMean(pressDurations.map(_._2)))
  }
  val timeBetweenKeys: Seq[(String, String, Double)] = if(keyPresses.length > 1) {
    keyPresses.sliding(2).map( x =>
    (x.head.letter, x.last.letter, x.last.startTime - x.head.endTime)).toSeq
  } else {
    Seq(("A", "A", 0.0)) //A little cheezy, but time between a single press means nothing so... set to 0.0
  }
  val averageTimeBetweenKeys: (Double, Double) = {
    (stats.mean(timeBetweenKeys.map(_._3)), stats.stdErrOnMean(timeBetweenKeys.map(_._3)))
  }
  val allStartTimes: Seq[Double] = keyPresses.map(x => x.startTime)
  val allEndTimes: Seq[Double] = keyPresses.map(x => x.endTime)

  //start of kpSequence: 1st press
  val startTime : Double = keyPresses.head.startTime

  //End of kpSequence: last release
  val endTime : Double = keyPresses.last.endTime
  val timeSpentToTypeSequence = {
    endTime - startTime
  }

  //Norms for the press durations and the interval between key presses.
  //Problem here is that by normalising, always get 0 or NaN for single letter words.
  val durationNorm: Double = stats.norm(keyPresses.map(_.keyPressDuration))
  val intervalNorm: Double = stats.norm(timeBetweenKeys.map(_.x._3))

  val toCSV : String = keyPresses.map(_.toCSV).mkString("", "\n", "")

  def compare(otherKPSequence: keyPressSequence) : Option[Double] = {
    if(rawInputString != otherKPSequence.rawInputString) {
      println("There is a mismatch between input texts.")
      None
    } else {
      val pairedKeyPresses = keyPresses zip otherKPSequence.keyPresses
      val durationDifference = 1 - pairedKeyPresses.map{ x =>
        x._1.keyPressDuration * x._2.keyPressDuration
      }.sum / (durationNorm * otherKPSequence.durationNorm)

      val bothTimesBetweenKeys = timeBetweenKeys zip otherKPSequence.timeBetweenKeys
      val intervalDifference = 1 - bothTimesBetweenKeys.map { x =>
        x._1._3 * x._2._3} .sum / (intervalNorm * otherKPSequence.intervalNorm)

      val aveDifference = (durationDifference + intervalDifference) / 2

      Some(aveDifference)
    }
  }

}
