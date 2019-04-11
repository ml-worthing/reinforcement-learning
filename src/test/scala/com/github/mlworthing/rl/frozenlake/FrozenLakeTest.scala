package com.github.mlworthing.rl
package frozenlake

import org.scalatest.{FreeSpec, Matchers}
import sun.management.resources.agent

class FrozenLakeTest extends FreeSpec with Matchers {

  "find a solution for K-armed bandit problem using epsilon greedy stationary agent" in {

    val frozenLake = FrozenLake(gamma = 1.0)
    val agent = new AgentMDP[Int, String]
    val policy = agent.solve(frozenLake)
    val reward = policy.runWith(frozenLake)

    reward shouldBe 1
  }
}
