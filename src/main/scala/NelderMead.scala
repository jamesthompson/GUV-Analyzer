package com.jamesrthompson.Fitting

import runtime._

class A[T <: B]

abstract class B {
  val mytab : Array[A[B]]
}

class NelderMead(f : Array[Double] => Array[Double], nbObjectives : Int, compare : (Array[Double], Array[Double]) => java.lang.Boolean, startP : Array[Double], dom : Array[Interval]) extends Optimizer {

  val dim = startP.length
  var startPoint = startP
  for(i <- 0 until dim) {
    if (!(dom(i).isInInterval(startPoint(i)))) throw new IllegalArgumentException("The starting point must be in the function domain.")
  }
  if (startPoint.length != dom.length) throw new IllegalArgumentException("The starting point and the domain intervals must be of the same dimension.")
  val deltaR = 1.0
  val deltaE = 2.0
  val deltaIC = -0.5
  val deltaOC = 0.5
  val gammaS = 0.5
  var iterCount = 0
  var noProgIterCount = 0
  val max = dom(0).size / 5.0
  var formerBest = Array.fill(nbObjectives) {
    scala.Double.MaxValue
  }
  var refCount = 0
  var refCount2 = 0
  var expCount = 0
  var conCount = 0
  var shrCount = 0
  evalCount = 0
  simplex = Array.ofDim[(Array[Double], Array[Double])](dim + 1)
  var currentBest : (Array[Double], Array[Double]) = simplex(0)

  def buildSimplex = {
    for(j <- 0 until dim)
      simplex(0) = (startPoint, f(startPoint))
    for(i <- 1 to dim) {
      val tmp = putInDomain(Array.tabulate(dim)(j => if (i == j + 1) startPoint(j) + dom(j).size / 20.0 else startPoint(j)))
      simplex(i) = (tmp, f(tmp))
    }
  }

  var formerSimplex = Array.ofDim[(Array[Double], Array[Double])](dim + 1)
  for(i <- 0 to dim) {
    formerSimplex(i) = (Array.ofDim[Double](dim), Array.fill(nbObjectives) {
      0.0
    })
  }

  var centroid = Array.ofDim[Double](dim)

  def reset = {
    iterCount = 0
    noProgIterCount = 0
    formerBest = Array.fill(nbObjectives) {
      Double.MaxValue
    }
    refCount = 0
    refCount2 = 0
    expCount = 0
    conCount = 0
    shrCount = 0
    evalCount = dim + 1
    buildSimplex
    copySimplex
  }

  reset

  def compareSimplex : Boolean = {
    for(i <- 0 to dim; if (!(formerSimplex(i).equals(simplex(i))))) {
      return false
    }
    true
  }

  def copySimplex = {
    for(i <- 0 to dim) {
      formerSimplex(i) = (simplex(i)._1, simplex(i)._2)
    }
  }

  def putInDomain(x : Array[Double]) : Array[Double] = {
    Array.tabulate(dim)(i => if (dom(i).isInInterval(x(i))) x(i) else dom(i).getClosestBound(x(i)))
  }

  def findCentroid : Array[Double] = {
    var tmpCentroid = Array.ofDim[Double](dim)
    for(i <- 0 until dim) {
      for(j <- 0 until dim)
        tmpCentroid(j) += simplex(i)._1(j)
    }
    val scaling = 1.0 / dim
    for(j <- 0 until dim)
      tmpCentroid(j) *= scaling
    tmpCentroid
  }

  def reflectPoint : (Array[Double], Array[Double]) = {
    val rPoint = putInDomain(Array.tabulate(dim)(i => centroid(i) + deltaR * (centroid(i) - simplex(dim)._1(i))))
    val eval = evaluator.eval(f, rPoint)
    evalCount += eval._2
    (rPoint, eval._1)
  }

  def expansPoint : (Array[Double], Array[Double]) = {
    val ePoint = putInDomain(Array.tabulate(dim)(i => centroid(i) + deltaE * (centroid(i) - simplex(dim)._1(i))))
    val eval = evaluator.eval(f, ePoint)
    evalCount += eval._2
    (ePoint, eval._1)
  }

  def contractPoint(inside : Boolean) : (Array[Double], Array[Double]) = {
    evalCount += 1
    val coef = if (inside) deltaIC else deltaOC
    val cPoint = putInDomain(Array.tabulate(dim)(i => centroid(i) + coef * (centroid(i) - simplex(dim)._1(i))))
    val eval = evaluator.eval(f, cPoint)
    evalCount += eval._2
    (cPoint, eval._1)
  }

  def shrink = {
    for(i <- 1 to dim) {
      evalCount += 1
      val curPoint = Array.tabulate(dim)(j => simplex(0)._1(j) + gammaS * (simplex(i)._1(j) - simplex(0)._1(j)))
      val eval = evaluator.eval(f, curPoint)
      evalCount += eval._2
      simplex(i) = (curPoint, eval._1)
    }
  }

  def insertInSimplex(newPoint : (Array[Double], Array[Double])) = {
    var tmpToCopy = (newPoint._1, newPoint._2)
    var tmpVal = (newPoint._1, newPoint._2)
    for(i <- 0 to dim - 1) {
      if (compare(newPoint._2, simplex(i)._2)) {
        tmpToCopy = (simplex(i)._1, simplex(i)._2)
        simplex(i) = (tmpVal._1, tmpVal._2)
        tmpVal = (tmpToCopy._1, tmpToCopy._2)
      }
    }
    simplex(dim) = (tmpVal._1, tmpVal._2)
  }

  def simplexDiameter : Double = {
    var maxDist = Double.MinValue
    for(i <- 1 to dim) {
      val tmpDist = pointDistance(simplex(i)._1, simplex(0)._1)
      maxDist = if (tmpDist > maxDist) tmpDist else maxDist
    }
    maxDist
  }

  def iter : List[(Array[Double], Array[Double])] = {
    val formerBest = simplex(0)
    var toRet = List[(Array[Double], Array[Double])]()
    if (iterNM) {
      scala.util.Sorting.stableSort(simplex, (pvp1 : (Array[Double], Array[Double]), pvp2 : (Array[Double], Array[Double])) => compare(pvp1._2, pvp2._2).booleanValue)
      var i = 0
      while (i < simplex.length && !areEqual(simplex(i)._1, formerBest._1)) {
        toRet = toRet ::: List(simplex(i))
        i += 1
      }
    }
    toRet
  }

  def iterNM : Boolean = {
    centroid = findCentroid
    val rPoint = reflectPoint
    val evalRPoint = rPoint._2
    val evalWorst = simplex(dim)._2
    val evalAlmostWorst = simplex(dim - 1)._2
    val evalBest = simplex(0)._2
    if ((compare(evalBest, evalRPoint) || (!compare(evalBest, evalRPoint) && !compare(evalRPoint, evalBest))) && compare(evalRPoint, evalAlmostWorst)) {
      insertInSimplex(rPoint)
      refCount += 1
      return false
    }
    else if (compare(evalRPoint, evalBest)) {
      val ePoint = expansPoint
      if (compare(ePoint._2, evalRPoint)) {
        insertInSimplex(ePoint)
        expCount += 1
        return false
      }
      else {
        insertInSimplex(rPoint)
        refCount2 += 1
        return false
      }
    }
    else if (!compare(evalRPoint, evalAlmostWorst)) {
      if (!compare(evalRPoint, evalWorst)) {
        val iCPoint = contractPoint(true)
        if (compare(iCPoint._2, evalWorst)) {
          insertInSimplex(iCPoint)
          conCount += 1
          return false
        }
        else {
          shrink
          shrCount += 1
          return true
        }
      }
      else {
        val oCPoint = contractPoint(false)
        if (compare(oCPoint._2, evalRPoint)) {
          insertInSimplex(oCPoint)
          conCount += 1
          return false
        }
        else {
          shrink
          shrCount += 1
          return true
        }
      }
    }
    true
  }

  override def optimize(tol : Double, evalLimit : Int, timeLimit : Int) : (Array[Double], Array[Double]) = {
    var needToSort = true
    var stop = false
    val begTime = System.currentTimeMillis()
    while (!stop) {
      if (needToSort) {
        scala.util.Sorting.stableSort(simplex, (pvp1 : (Array[Double], Array[Double]), pvp2 : (Array[Double], Array[Double])) => compare(pvp1._2, pvp2._2).booleanValue)
        needToSort = false
      }
      if (simplexDiameter < tol) {
        return simplex(0)
      }

      needToSort = iterNM
      iterCount += 1
      if (compareSimplex || evalCount >= evalLimit || (System.currentTimeMillis() - begTime) / 1000 >= timeLimit) {
        return simplex(0)
      }
      copySimplex
      if (compare(simplex(0)._2, formerBest)) {
        currentBest = simplex(0)
        onImprovement()
        formerBest = simplex(0)._2
        noProgIterCount = 0
      }
      else {
        noProgIterCount += 1
      }
      if (noProgIterCount >= dim * 10) {
        return simplex(0)
      }
    }
    simplex(0)
  }

  override def sampledOptimize(nbSamples : Int, tol : Double) : (Array[Double], Array[Double]) = {
    var fctCallCount = 0
    val samplePoints = sampler.scrambledHaltonSequence(nbSamples, dom)
    var bestResult = (Array.fill(dom.length) {
      0.0
    }, Array.fill(nbObjectives) {
      scala.Double.MaxValue
    })
    for(i <- 0 until nbSamples) {
      startPoint = samplePoints(i)
      reset
      val curResult = optimize(tol, 10000, 100)
      fctCallCount += evalCount
      if (compare(curResult._2, bestResult._2))
        bestResult = (curResult._1, curResult._2)
    }
    evalCount = fctCallCount
    bestResult
  }
}

object NelderMead {
  def apply(f : Array[Double] => Array[Double], nbObjectives : Int, compare : (Array[Double], Array[Double]) => java.lang.Boolean,
            startP : Array[Double], dom : Array[Interval]) = new NelderMead(f, nbObjectives, compare, startP, dom)
}
