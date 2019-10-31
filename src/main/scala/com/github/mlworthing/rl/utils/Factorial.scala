package com.github.mlworthing.rl.utils

object Factorial {

  def apply(n: Int): Long = {
    assert(n >= 0)
    factorial(n)
  }

  private val factorial: Map[Int, Long] = Map(
    0  -> 1L,
    1  -> 1L,
    2  -> 2L,
    3  -> 6L,
    4  -> 24L,
    5  -> 120L,
    6  -> 720L,
    7  -> 5040L,
    8  -> 40320L,
    9  -> 362880L,
    10 -> 3628800L,
    11 -> 39916800L,
    12 -> 479001600L,
    13 -> 6227020800L,
    14 -> 87178291200L,
    15 -> 1307674368000L,
    16 -> 20922789888000L,
    17 -> 355687428096000L,
    18 -> 6402373705728000L,
    19 -> 121645100408832000L,
    20 -> 2432902008176640000L
  ).withDefault(n => factorial(n - 1) * n)
}
