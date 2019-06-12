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

package com.github.mlworthing.rlai

import java.lang.Math._

import com.github.mlworthing.rlai.utils.MathNotation._


/**
  * The object providing the API for solving MDP problems
  */
object Mdp {


  /**
    * Iterative Policy Evaluation, for estimating V which follows the policy.
    * Based on http://incompleteideas.net/book/RLbook2018trimmed.pdf, page 75
    * It mutates the v
    */
  def evaluatePolicy[S, A](v: ValueFunction[S],
                           π: Policy[S, A],
                           theta: Double = 0.01 //acceptable error
                          )(implicit c: MdpContext[S, A]): Unit = {
    import c._

    var error = 0.0
    do {
      error = 0.0
      states.nonTerminalStates.foreach { s =>
        val oldV = v(s)

        v(s) = Σ(actions(s))((a: A) =>
          π(s, a) * Σ(ś(s, a), rewards(s, a))((ś: S, r: R) =>
            p(ś, r, s, a) * (r + γ * v(ś)))
        )
        error += abs(oldV - v(s))
      }
    } while (error > theta)
  }

  /**
    * Policy Iteration algorithm, http://incompleteideas.net/book/RLbook2018trimmed.pdf, page 80
    */
  def iteratePolicy[S, A](v: ValueFunction[S],
                          π: Policy[S, A]
                         )(implicit c: MdpContext[S, A]): Unit = {
    import c._

    val acceptableError = 0.01
    var isPolicyStable = false

    def setPolicyStable(old: ImmutablePolicy[S,A], current: ImmutablePolicy[S,A]): Unit = if (old.diff(current) <= acceptableError ) isPolicyStable = true

    def improvePolicy(): Unit = {
      val oldPolicy = π.asImmutable()
      (0 to 100).foreach { _ =>
        states.nonTerminalStates.foreach { s: S =>
          π(s) = argmax(actions(s))(a => Σ(ś(s, a), rewards(s, a))((ś, r) => p(ś, r, s, a) * (r + γ * v(ś))))
        }
      }
      setPolicyStable(oldPolicy, π.asImmutable())
    }

    do {
      evaluatePolicy(v, π)
      improvePolicy()
    } while (!isPolicyStable)
    ()
  }

  def iteratePolicy[S,A]()(implicit c: MdpContext[S,A]): Policy[S, A] = {
    val π: Policy[S, A] = Policy.createRandomPolicy()
    val v: ValueFunction[S] = ValueFunction.createRandomValueFunction[S, A]()

    iteratePolicy(v, π)
    π
  }
  /**
    * Value Iteration algorithm for estimating Policy to be the BestPolicy,
    * http://incompleteideas.net/book/RLbook2018trimmed.pdf, page 83
    */
  def iterateValue[S, A](theta: Double = 0.01 //acceptable error
                        )(implicit c: MdpContext[S, A]): Policy[S, A] = {
    import c._

    val v: ValueFunction[S] = ValueFunction.createRandomValueFunction[S,A]()

    var error = 0.0
    do {
      error = 0.0
      states.nonTerminalStates.foreach { s =>
        val oldV = v(s)
        v(s) = max_(actions(s))(a =>
          Σ(ś(s, a), rewards(s, a))((ś, r) => p(ś, r, s, a) * (r + γ * v(ś)))
        )
        error += abs(oldV - v(s))
      }
    } while (error > theta)

    val π = Policy.createPolicy(v)
    π
  }


}

