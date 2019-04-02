package com.github.mlworthing.rl.bandit

import org.scalatest.FreeSpec

class KArmedBanditTest extends FreeSpec {

  "find a solution for K-armed bandit problem using epsilon greedy agent" in {

    val arms = Map(
      1 -> (0, 10),
      2 -> (1, 1),
      5 -> (2, 4),
      7 -> (3, 2),
      9 -> (-1, 2)
    )

    val kArmedBandit = new KArmedBandit(arms)
    val eGreedyAgent = EGreedyAgent(epsilon = 0.1, rate = 1, stepsToLearn = 200)
    val solution = eGreedyAgent.solve(kArmedBandit)

    println(solution.head)
  }
}
