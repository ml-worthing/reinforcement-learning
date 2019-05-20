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

import com.github.mlworthing.rl.mdp.mdpalgorithm.{Mdp, MdpContext, MdpDescription, States}
import com.github.mlworthing.rl.utils.PolicyExecutor

import scala.util.Random

class FrozenLakeTest extends UnitSpec {

  "evaluate a policy for a Frozen Lake using AgentSimpleMDP" in {

    val agent = new AgentSimpleMDP[Int, String](gamma = 0.9d, theta = 0.01d, maxIterations = 100)
    val policy: Deterministic[Int, String] = agent.solve(FrozenLake)

    PolicyExecutor.execute(policy, FrozenLake, maxIterations = 1000, numberOfSamples = 1000)
  }

  "evaluate a policy for a Frozen Lake using AgentMDP" in {

    val agent = new AgentMDP[Int, String](gamma = 0.9d, theta = 0.01d, maxIterations = 100)
    val policy: Deterministic[Int, String] = agent.solve(FrozenLake)

    PolicyExecutor.execute(policy, FrozenLake, maxIterations = 1000, numberOfSamples = 1000)
  }

  "Solve FrozenLake using Mdp" in {

    type State = Int
    type Action = String

    val nonTerminalStates: Set[State] = FrozenLake.board.keySet diff FrozenLake.terminalStates.toSet

    val mdpDescription = MdpDescription[State, Action](
      states = States(
        terminalStates = FrozenLake.terminalStates,
        nonTerminalStates = nonTerminalStates
      ),
      actions = {
        case s if nonTerminalStates.contains(s) => FrozenLake.actions
        case s                                  => Nil
      },
      rewards = {
        case (s, a) => FrozenLake.board(s)(a).map(_._3)
      },
      p = {
        case (ś, r, s, a) => FrozenLake.board(s)(a).find(x => x._1 == ś && x._3 == r).map(_._2).getOrElse(0.0)
      }
    )

    implicit val c: MdpContext[State, Action] = MdpContext(
      mdpDescription = mdpDescription,
      γ = 0.99,
      random = new Random(121)
    )

    val π1 = Mdp.iteratePolicy()

    println(
      FrozenLake.show(
        s => Some(π1(s)),
        (_: State, action: Action) => action.toString,
        cellLength = 1,
        showForTerminalTiles = false)
    )


    val π2 = Mdp.iterateValue(0.00001)

    println(
      FrozenLake.show(
        s => Some(π2(s)),
        (_: State, action: Action) => action.toString,
        cellLength = 1,
        showForTerminalTiles = false)
    )

    //TODO: this computes different policies for different randoms, something is still not working

    //below policy computed by Artur's implementation
    //← ↑ ↑ ↑
    //← F → F
    //↑ ↓ ← F
    //F → ↓ G

  }
}
