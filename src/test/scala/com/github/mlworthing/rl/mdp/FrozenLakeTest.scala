package com.github.mlworthing.rl
package mdp

import org.scalatest.{FreeSpec, Matchers}
import sun.management.resources.agent

class FrozenLakeTest extends FreeSpec with Matchers {

  "evaluate a random policy in a Frozen Lake" in {

    val frozenLake = FrozenLake(gamma = 1.0)
    val agent = new AgentSimpleMDP[Int, String](theta = 0.1)
    val policy = agent.solve(frozenLake)
    val reward = policy.runWith(frozenLake)

    //reward shouldBe 1
  }
}
