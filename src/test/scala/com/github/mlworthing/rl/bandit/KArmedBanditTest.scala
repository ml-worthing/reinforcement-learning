package com.github.mlworthing.rl.bandit

import com.github.mlworthing.rl.utils.AgentEvaluator
import org.scalatest.{FreeSpec, Matchers}

class KArmedBanditTest extends FreeSpec with Matchers {

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

    val evaluator = AgentEvaluator(
      expected = Seq(7),
      agent = EGreedyAgent(0.1, 1, _),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000)
    )

    val stats = evaluator.evaluate(kArmedBandit, 100)

    stats.maxBy(_._2)._2 should be > 99d
  }
}
