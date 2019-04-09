package com.github.mlworthing.rl
package bandit

import com.github.mlworthing.rl.utils.AgentEvaluator
import org.scalatest.{FreeSpec, Matchers}
import sun.management.resources.agent

class KArmedBanditTest extends FreeSpec with Matchers {

  val arms = Map(
    1 -> (0d, 10d),
    2 -> (1d, 2d),
    5 -> (2d, 4d),
    7 -> (3d, 3d),
    9 -> (-1d, 3d)
  )

  "find a solution for K-armed bandit problem using epsilon greedy stationary agent" in {

    val kArmedBandit = new KArmedBandit(arms)

    val evaluator = AgentEvaluator(
      expected = Winner(7),
      agent = EpsilonGreedyStationaryProblemAgent[Int](0.2, _),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon greedy stationary agent\nwith regard to the number of steps"
    )

    val stats = evaluator.evaluate(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.0d
  }

  "find a solution for K-armed bandit problem using epsilon greedy non-stationary agent" in {

    val kArmedBandit = new KArmedBandit(arms)

    val evaluator = AgentEvaluator(
      expected = Winner(7),
      agent = EpsilonGreedyNonStationaryProblemAgent[Int](0.2, 0.1, _),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon greedy non-stationary agent\nwith regard to the number of steps"
    )

    val stats = evaluator.evaluate(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.0d
  }
}
