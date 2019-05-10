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

package com.github.mlworthing.rl.mdp.mdpalgorithm

import java.lang.Math._

import com.github.mlworthing.rl.utils.MathNotation._

import scala.collection.mutable

/**
  * The object providing the API for solving MDP problems
  */
object Mdp {

  /**
    * Create a value-function which always results in zeros
    */
  def createValueFunction0[S, A]()(implicit c: MdpContext[S,A]): mutable.Map[S, P] = {
    import c.mdpDescription._
    (mutable.Map.newBuilder[S, Double] ++= states.map(s => (s, 0.0))).result()
  }


  /**
    * Iterative Policy Evaluation, for estimating V which follows the policy.
    * Based on http://incompleteideas.net/book/RLbook2018trimmed.pdf, page 75
    * It mutates the v
    */
  def evaluatePolicy[S,A](v: mutable.Map[S, R],
                              π: Policy[S, A],
                              theta: Double = 0.01 //acceptable error
                             )(implicit c: MdpContext[S,A]): Unit = {
    import c._
    import mdpDescription._

    var delta = 0.0
    do {
      states.foreach { s =>
        val oldV = v(s)

        v(s) = Σ(actions(s))((a: A) =>
          π(s, a) * Σ(ś(s, a), rewards(s, a))((ś: S, r: R) =>
            p(ś, r, s, a) * (r + γ * v(ś)))
        )
        delta = max(delta, abs(oldV - v(s)))
      }
    } while (delta < theta)
  }


  /**
    * Policy Iteration algorithm, http://incompleteideas.net/book/RLbook2018trimmed.pdf, page 80
    */
  def iteratePolicy[S,A](v: mutable.Map[S, R],
                         π: Policy[S, A]
                   )(implicit c: MdpContext[S,A]): Unit = {
    import c._
    import mdpDescription._

    var isPolicyStable = true


    def setPolicyStable(oldAction: A, currentAction: A): Unit = if (oldAction == currentAction) isPolicyStable = true

    def improvePolicy(): Unit = states.foreach { s: S =>
      val oldAction = π.greedyAction(s)
      π(s) = argmax(actions(s))(a => Σ(ś(s, a), rewards(s, a))((ś, r) => p(ś, r, s, a) * (r + γ * v(ś))))
      setPolicyStable(oldAction, π.greedyAction(s))
    }

    do {
      evaluatePolicy(v, π)
      improvePolicy()
    } while (!isPolicyStable)
  }


}

