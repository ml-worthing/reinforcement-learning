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

package com.github.mlworthing.rl.problems

import com.github.mlworthing.rl.Deterministic
import com.github.mlworthing.rl.agents.{AgentMDP, DynamicProgrammingEvaluateImproveAgent, DynamicProgrammingValueIterationAgent}
import com.github.mlworthing.rl.utils.PolicyExecutor
import com.github.mlworthing.rlai.utils.UnitSpec

class FrozenLakeTest extends UnitSpec {

  "evaluate a policy for a Frozen Lake using DynamicProgrammingEvaluateImproveAgent" in {

    val agent =
      new DynamicProgrammingEvaluateImproveAgent[Int, String](gamma = 0.9d, theta = 0.01d, maxIterations = 100)
    val policy: Deterministic[Int, String] = agent.solve(FrozenLake)

    val result = PolicyExecutor.execute(policy, FrozenLake, maxIterations = 1000, numberOfSamples = 1000)
    result.getOrElse(1.0, 0) should be > 0 withClue ": all of policy executions have failed to reach the goal"
  }

  "evaluate a policy for a Frozen Lake using DynamicProgrammingValueIterationAgent" in {

    val agent =
      new DynamicProgrammingValueIterationAgent[Int, String](gamma = 0.9d, theta = 0.01d, maxIterations = 100)
    val policy: Deterministic[Int, String] = agent.solve(FrozenLake)

    val result = PolicyExecutor.execute(policy, FrozenLake, maxIterations = 1000, numberOfSamples = 1000)
    result.getOrElse(1.0, 0) should be > 0 withClue ": all of policy executions have failed to reach the goal"
  }

  "evaluate a policy for a Frozen Lake using AgentMDP" ignore {

    val agent = new AgentMDP[Int, String](gamma = 0.9d, theta = 0.01d, maxIterations = 100)
    val policy: Deterministic[Int, String] = agent.solve(FrozenLake)

    PolicyExecutor.execute(policy, FrozenLake, maxIterations = 1000, numberOfSamples = 1000)
  }
}
