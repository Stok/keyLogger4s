package keytypeAnalysis

/**
  *
  */
object eventParser {

  //Basic preparation for analysis. Returns a sequence of keyPresses.
  def parseText(eventTextInput: String): keyPressSequence = {
    val splitString = eventTextInput.split("\n").map(_.split(" ").map(_.trim))
    val events = splitString.map(x => {
      if (x.head == "p") {
        (true, x(1), x(2).dropRight(6).toInt / 1000.0)
      } else {
        (false, x(1), x(2).dropRight(6).toInt / 1000.0)
      }
    })
    //.filterNot(_._2 == "⇧")
    //Press = true, Release = false

    val kPs = pairPressAndRelease(events).map(x => new keyPress(x._2, x._3, x._1))

    //No longer needed as we're no longer separating by words
    /*def splitAtAllSpaces (data: Seq[keyPress]): Seq[Seq[keyPress]] = {
      def ss(data: Seq[keyPress]): Seq[Seq[keyPress]] = {
        val indices = data.filter(_.letter == "␣").map(x => data.indexOf(x))
        if (indices.nonEmpty) {
          val split = Seq(data.take(indices.head), data.drop(indices.head + 1))
          val result = ss(split.last)
          Seq(split.head) ++ result
        }
        else {
          if(data.nonEmpty) {
            Seq(data, Seq())
          } else {
            Seq()
          }
        }
      }
      ss(data).dropRight(1).map(x => x.filterNot(_.letter == "␣"))
    }*/

    return keyPressSequence(kPs)
  }

  //This pairs up the press and release time of a key.
  private def pairPressAndRelease(rawData: Seq[(Boolean, String, Double)]): Seq[(String, Double, Double)] /*Seq[(String, Int, Int)]*/ = {
    //Separate out the presses and the releases from the console string
    val presses = rawData.filter(_._1 == true).map(x => (x._2, x._3))
    val releases = rawData.filter(_._1 == false).map(x => (x._2, x._3))
    return pp(presses, releases)
  }
  //Recursive function to pair up the press and release
  private def pp(presses: Seq[(String, Double)], releases: Seq[(String, Double)]): Seq[(String, Double, Double)] = {
    if (presses.length > 1) {
      //If first press and release are a match, move on to next character. Fold back on return
      if (presses(0)._1 == releases(0)._1) {
        val result = pp(presses.drop(1), releases.drop(1))
        val localResult = (presses(0)._1, presses(0)._2, releases(0)._2)
        return Seq(localResult) ++ result
      }
        //Take up to 3 subsequent strokes and see if any permutation of the releases match the press order
      else {
        val takeLength = if (presses.length > 3) 3 else presses.length
        val subPresses = presses.take(takeLength)
        val subReleases = releases.take(takeLength)
        val permutations = subReleases.permutations.toArray
        val newOrder: Seq[(String, Double)] = permutations(permutations.map { x =>
          checkOverallMatch(x, subPresses)
        }.indexOf(true))
        val localResult =
          for (i <- 0 to subPresses.length - 1) yield {
            (subPresses(i)._1, subPresses(i)._2, newOrder(i)._2)
          }
        val result = pp(presses.drop(takeLength), releases.drop(takeLength))
        return localResult ++ result
      }

    } else {
      if (releases.length == 1) {
        return Seq((presses(0)._1, presses(0)._2, releases(0)._2))
      }
      else return Seq()
    }
  }
  //Helper functions to test for equality by the key_value
  private def checkOverallMatch(x: Seq[(String, Double)], y: Seq[(String, Double)]): Boolean = {
    val eachMatch = for (i <- 0 to x.length - 1) yield {
      checkMatch(x(i), y(i))
    }
    return eachMatch.filter(_ == true).nonEmpty
  }
  private def checkMatch(x: (String, Double), y: (String, Double)): Boolean = {
    if (x._1 == y._1) {
      true
    }
    else false
  }

}


