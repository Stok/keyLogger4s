package keytypeAnalysis

/**
  *
  */
object stats {
  def mean(items: Seq[Double]): Double = {
    items.sum / items.length
  }
  def stdDev(items: Seq[Double]): Double = {
    val ave = mean(items)
    if (items.length > 1) {
      math.sqrt(items.map(x => (x - ave) * (x - ave)).sum / (items.length - 1))
    }
    else 0.0
  }
  def stdErrOnMean(items: Seq[Double]) : Double = {
    stdDev(items) / math.sqrt(items.length)
  }
  //Only reals
  def norm(vec: Seq[Double]) : Double = {
    math.sqrt(vec.map{x => x * x}.sum)
  }

}
