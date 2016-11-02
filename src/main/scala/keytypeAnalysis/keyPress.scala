package keytypeAnalysis

/**
  *
  */
case class keyPress (startTime: Double, endTime: Double, letter: String) {
  val keyPressDuration = endTime - startTime

  val toCSV: String = letter + ", " + startTime + ", " + endTime
  //times in ms
}
