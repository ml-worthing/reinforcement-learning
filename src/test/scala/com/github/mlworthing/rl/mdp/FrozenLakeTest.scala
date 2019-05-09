/*
 * Copyright 2019 ml-worthing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.mlworthing.rl
package mdp

import org.scalatest.{FreeSpec, Matchers}
import sun.management.resources.agent

class FrozenLakeTest extends FreeSpec with Matchers {

  "evaluate a random policy in a Frozen Lake" in {

    val frozenLake = FrozenLake(gamma = 1.0)
    val agent = new AgentSimpleMDP[Int, String](gamma = 0.9d, theta = 0.01d, maxIterations = 100)
    val policy = agent.solve(frozenLake)
    val reward = policy.runWith(frozenLake, maxIterations = 100)
    println(s"Evaluated reward: $reward")
  }
}
