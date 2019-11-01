package com.github.mlworthing.rl.utils

/**
  * Discrete probability distribution that expresses the probability
  * of a given number of events occurring in a fixed interval of time.
  * @see https://en.wikipedia.org/wiki/Poisson_distribution
  *
  * Caches Poisson's distribution given an expected value (lambda).
  */
case class PoissonDistribution(lambda: Double) {

  private val distribution: Cache[Int, Double] = Cache(probability)

  /**
    * Returns Poisson's probability of value n
    */
  def apply(n: Int): Double = distribution(n)

  /**
    * Returns Poisson's probability of value n with a cap on max value,
    * where probability of max is 1 minus sum of probabilities from 0 until max.
    */
  def apply(n: Int, max: Int): Double =
    if (n < max) distribution(n)
    else 1d - (0 until max).map(distribution(_)).sum

  def probability(n: Int): Double =
    Math.pow(lambda, n) / Factorial(n) * Math.exp(-lambda)
}
