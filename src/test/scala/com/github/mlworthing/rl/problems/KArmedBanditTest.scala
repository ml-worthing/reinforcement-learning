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

import com.github.mlworthing.rl.Winner
import com.github.mlworthing.rl.agents.singlestate.{EpsilonGreedyConstantStepSizeAgent, EpsilonGreedySampleAverageAgent, EpsilonGreedyUnbiasedConstantStepSizeAgent, GreedySampleAverageAgent, StochasticGradientAscentAgent, UpperConfidenceBoundGreedyAgent}
import com.github.mlworthing.rl.environments.SingleStateEnvironment
import com.github.mlworthing.rl.utils.{AgentExecutor, ExecutionResults}
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}
import sun.management.resources.agent

class KArmedBanditTest extends FreeSpec with Matchers with BeforeAndAfterAll {

  val numberOfRuns = 100

  def stationaryArms = Map(
    1 -> BanditArm.stationary.gaussian(mean = 0d, range = 10d, deviation = 7d),
    2 -> BanditArm.stationary.gaussian(mean = 1d, range = 2d, deviation = 2d),
    5 -> BanditArm.stationary.uniform(mean = 2d, range = 4d /*, deviation = 1.5d*/ ),
    7 -> BanditArm.stationary.gaussian(mean = 2.5d, range = 3d, deviation = 2d),
    9 -> BanditArm.stationary.uniform(mean = -1d, range = 2d)
  )

  def nonStationaryArms = Map(
    1 -> BanditArm.stationary.gaussian(mean = 0d, range = 10d, deviation = 7d),
    2 -> BanditArm.stationary.gaussian(mean = 1d, range = 2d, deviation = 2d),
    5 -> BanditArm.drifting.uniform(mean = 2d, range = 4d /*, deviation = 1.5d*/, drift = 0.01d),
    7 -> BanditArm.drifting.gaussian(mean = 1d, range = 3d, deviation = 2d, drift = 1d),
    9 -> BanditArm.stationary.uniform(mean = -1d, range = 2d)
  )

  val stationaryBandit: KArmedBandit[Int] = new KArmedBandit(stationaryArms)
  def nonStationaryBandit: KArmedBandit[Int] = new KArmedBandit(nonStationaryArms)

  class KArmedBanditExecution[C](
    underTest: => KArmedBandit[Int],
    executor: AgentExecutor[Unit, Int, C, SingleStateEnvironment[Int]]) {
    val stats: ExecutionResults[C] = executor.execute(underTest, numberOfRuns)
    stats.print
    stats.maxRate should be > 99.0d
  }

  "do not find a solution for a stationary K-armed bandit problem using greedy sample-average agent with variable number of steps" in {
    val executor = AgentExecutor(
      expected = Winner(7),
      agent = GreedySampleAverageAgent[Int](_, initialValue = 0d),
      configurations = Seq(5, 10, 25, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of greedy sample-average agent\nwith regard to the number of steps",
      "steps"
    )
    val stats = executor.execute({ stationaryBandit }, numberOfRuns)
    stats.print
    stats.maxRate shouldBe 0
  }

  "find a solution for a stationary K-armed bandit problem using epsilon-greedy sample-average agent with variable number of steps" in new KArmedBanditExecution(
    underTest = { stationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedySampleAverageAgent[Int](0.02, _, initialValue = 0d),
      configurations = Seq(5, 10, 25, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon-greedy sample-average agent (epsilon=0.02)\nwith regard to the number of steps",
      "steps"
    )
  )

  "find a solution for a stationary K-armed bandit problem using epsilon-greedy sample-average agent with variable exploitation factor (epsilon)" in new KArmedBanditExecution(
    underTest = { stationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedySampleAverageAgent[Int](_, 1000, initialValue = 0d),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon-greedy sample-average agent (stepsToLearn=1000)\nwith regard to the exploitation factor (epsilon)",
      "epsilon"
    )
  )

  "find a solution for a non-stationary K-armed bandit problem using epsilon-greedy constant-step agent with variable number of steps" in new KArmedBanditExecution(
    underTest = { nonStationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyConstantStepSizeAgent[Int](0.02, 0.1, _, initialValue = 0d),
      configurations = Seq(5, 10, 25, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon-greedy constant-step agent (epsilon=0.02, constantStepSize=0.1)\nwith regard to the number of steps",
      "steps"
    )
  )

  "find a solution for a non-stationary K-armed bandit problem using epsilon-greedy constant-step agent with variable exploitation factor (epsilon)" in new KArmedBanditExecution(
    underTest = { nonStationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyConstantStepSizeAgent[Int](_, 0.2, 1000, initialValue = 0d),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon-greedy constant-step agent (stepsToLearn=1000)\nwith regard to the exploitation factor (epsilon)",
      "epsilon"
    )
  )

  "find a solution for a non-stationary K-armed bandit problem using epsilon-greedy constant-step agent with variable step size" in new KArmedBanditExecution(
    underTest = { nonStationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyConstantStepSizeAgent[Int](0.02, _, 1000, initialValue = 0d),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon-greedy constant-step agent (stepsToLearn=1000,epsilon=0.02)\nwith regard to the step size",
      "step size"
    )
  )

  "find a solution for a non-stationary K-armed bandit problem using epsilon-greedy unbiased constant-step agent with variable step size" in new KArmedBanditExecution(
    underTest = { nonStationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyUnbiasedConstantStepSizeAgent[Int](0.02, _, 1000, initialValue = 0d),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon-greedy unbiased constant-step agent (stepsToLearn=1000,epsilon=0.02)\nwith regard to the step size",
      "step size"
    )
  )

  "find a solution for a non-stationary K-armed bandit problem using epsilon-greedy constant-step agent with variable number of steps and non-zero initial value" in new KArmedBanditExecution(
    underTest = { nonStationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyConstantStepSizeAgent[Int](0.02, 0.1, _, initialValue = 10d),
      configurations = Seq(5, 10, 25, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon-greedy constant-step agent (epsilon=0.02)\nwith regard to the number of steps and non-zero initial value",
      "steps"
    )
  )

  "find a solution for a stationary K-armed bandit problem using upper-confidence-bound greedy sample-average agent with variable number of steps" in new KArmedBanditExecution(
    underTest = { stationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent =
        UpperConfidenceBoundGreedyAgent[Int](explorationFactor = 0.8, _, initialValue = 0d, constantStepSize = None),
      configurations = Seq(5, 10, 25, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of upper-confidence-bound greedy sample-average agent (c=0.8)\nwith regard to the number of steps",
      "steps"
    )
  )

  "find a solution for a stationary K-armed bandit problem using upper-confidence-bound greedy sample-average agent with variable exploration factor (c)" in new KArmedBanditExecution(
    underTest = { stationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent = UpperConfidenceBoundGreedyAgent[Int](_, 100, initialValue = 0d, constantStepSize = None),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of upper-confidence-bound greedy sample-average agent (stepsToLearn=100)\nwith regard to the exploration factor (c)",
      "c"
    )
  )

  "find a solution for a non-stationary K-armed bandit problem using stochastic-gradient-ascent constant step size agent with variable number of steps" in new KArmedBanditExecution(
    underTest = { nonStationaryBandit },
    executor = AgentExecutor(
      expected = Winner(7),
      agent = StochasticGradientAscentAgent[Int](alpha = 0.2, _, initialValue = 0d, constantStepSize = Some(0.1)),
      configurations = Seq(5, 10, 25, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of stochastic-gradient-ascent constant step size agent (c=0.2)\nwith regard to the number of steps",
      "steps"
    )
  )
}
