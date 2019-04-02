package com.github.mlworthing.rl.bandit

import org.scalatest.FreeSpec

class BanditTest extends FreeSpec {

  "find a solution for Bandit problem using e-greedy agent" in {

    val arms = Map(
      1 -> (0, 10),
      2 -> (1, 1),
      5 -> (2, 4),
      7 -> (3, 2),
      9 -> (-1, 2)
    )

    val bandit = new Bandit(arms)
    val eGreedyAgent = EGreedyAgent(epsilon = 0.1, rate = 1, stepsToLearn = 200)
    val solution = eGreedyAgent.solve(bandit)

    println(solution.head)
  }
}
