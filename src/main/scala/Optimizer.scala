package com.jamesrthompson.Fitting

trait Optimizer {

  var evalCount = 0
  val evaluator = new DeterministicEvaluation()
  val rand = Optimizer.randGen
  val sampler = Optimizer.samplerGen
  var simplex = Array[(Array[Double], Array[Double])]()
  var currentBest : (Array[Double], Array[Double])

  def optimize(tol : Double, evalLimit : Int, timeLimit : Int) : (Array[Double], Array[Double])

  def sampledOptimize(nbSamples : Int, tol : Double) : (Array[Double], Array[Double])

  def putInDomain(x : Array[Double]) : Array[Double]

  def pointToStr(point : Array[Double]) : String = {
    var res = "("
    for(i <- 0 to point.length - 2)
      res += point(i) + ", "
    res += point(point.length - 1) + ")"
    res
  }

  def iter() : List[(Array[Double], Array[Double])]

  def confIntervSize(alpha : Double, tol : Double, max : Double) : Double = {
    val normalizedAlpha = (alpha / tol) + tol
    math.max(tol, math.log(normalizedAlpha) / math.log(max))
  }

  def areEqual(p1 : Array[Double], p2 : Array[Double]) : Boolean = {
    for(i <- 0 until p1.length) {
      if (p1(i) != p2(i))
        return false
    }
    true
  }

  var maxAlpha = Array(Double.MaxValue)
  var alpha = Array.tabulate(1)(i => 0.0)

  def simplexDiameter : Double

  var onImprovement : () => Unit = () => {}

  def pointDistance(p1 : Array[Double], p2 : Array[Double]) : Double = {
    var norm = 0.0
    for(i <- 0 until p1.length)
      norm += math.pow((p2(i) - p1(i)), 2)
    math.sqrt(norm)
  }
}

object Optimizer {
  val randGen = new scala.util.Random(0)
  val samplerGen = new QuasiRandomSequence(randGen)
}
