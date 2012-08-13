package com.jamesrthompson.Fitting

trait Evaluation {

  def eval(f : Array[Double] => Array[Double], point : Array[Double]) : (Array[Double], Int) = {
    (f(point), 1)
  }
}

class DeterministicEvaluation extends Evaluation