package com.github.mlworthing.rl.bandit

import com.github.mlworthing.rl.utils.AgentEvaluator
import org.scalatest.{FreeSpec, Matchers}
import sun.management.resources.agent

class KArmedBanditTest extends FreeSpec with Matchers {

  "find a solution for K-armed bandit problem using epsilon greedy agent" in {

    val arms = Map(
      1 -> (0d, 10d),
      2 -> (1d, 2d),
      5 -> (2d, 4d),
      7 -> (3d, 3d),
      9 -> (-1d, 3d)
    )

    val kArmedBandit = new KArmedBandit(arms)

    val evaluator = AgentEvaluator(
      expected = Seq(7),
      agent = EGreedyAgent[Int](0.2, 1, _),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000)
    )

    val stats = evaluator.evaluate(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.9d
  }
}
