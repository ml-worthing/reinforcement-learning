package com.github.mlworthing.rl.utils

import scala.util.Random

/**
  * Sample Gaussian distributed numbers.
  * @see <https://www.alanzucconi.com/2015/09/16/how-to-sample-from-a-gaussian-distribution/>
  */
object GaussianRandom {

  def next(): Double = {
    var v1 = 0d
    var v2 = 0d
    var s = 0d
    do {
      v1 = 2.0 * Random.nextDouble() - 1.0
      v2 = 2.0 * Random.nextDouble() - 1.0
      s = v1 * v1 + v2 * v2
    } while (s >= 1.0 || s == 0.0)
    s = Math.sqrt((-2.0 * Math.log(s)) / s)
    v1 * s
  }

  def next(mean: Double, deviation: Double): Double = mean + next() * deviation

  def next(mean: Double, deviation: Double, min: Double, max: Double): Double = {
    var value = mean
    do {
      value = next(mean, deviation)
    } while (value < min || value > max)
    value
  }
}
