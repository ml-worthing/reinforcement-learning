package com.github.mlworthing.rl.utils

/**
  * Discrete probability distribution that expresses the probability
  * of a given number of events occurring in a fixed interval of time.
  *
  * @see https://en.wikipedia.org/wiki/Poisson_distribution
  *
  * Creates Poisson's distribution given an expected value and a range of possible values.
  * Probability of minimum value is a sum of probabilities from 0 to min.
  * Probability of maximum value is a difference between 1 and sum of probabilities from 0 until max.
  * Probability of a value outside range is 0.
  */
case class PoissonDistribution(expected: Int) {

  private val distribution: Cache[Int, Double] = Cache(probability(_, expected))

  def apply(n: Int): Double = distribution(n)

  /**
    * Returns Poisson's probability of n with a cap on max value,
    * where probability of max is 1 minus sum of probabilities from 0 until max.
    */
  def apply(n: Int, max: Int): Double = if (n < max) distribution(n) else 1d - (0 until max).map(distribution(_)).sum

  def probability(n: Int, lambda: Int): Double =
    Math.pow(lambda, n) / Factorial(n) * Math.exp(-lambda)
}
