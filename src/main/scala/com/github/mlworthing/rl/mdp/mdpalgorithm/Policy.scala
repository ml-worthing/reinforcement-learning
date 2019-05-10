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

import java.lang.Math.abs

import com.github.mlworthing.rl.utils.MathNotation.argmax

import scala.collection.mutable
import scala.util.Random

/**
  * Mutable Policy.
  */
class Policy[S, A](
                    mdpDescription: MdpDescription[S, A],
                    πpFactory: MdpDescription[S, A] => Policy.ΠP[S, A]
                  ) {

  import mdpDescription._

  private val πp = πpFactory(mdpDescription)

  /**
    * Update policy so it promotes action 'a' for a given state 's'
    * Such syntax available: 'policy(s) = a'
    */
  def update(s: S, a: A): Unit = {
    actions(s).foreach(a => πp((s, a)) = 0.0)
    πp((s, a)) = 1.0
  }

  /**
    * Returns a probability of selecting action in a given state 's'
    */
  def apply(s: S, a: A): P = πp((s, a))

  def greedyAction(s: S): A = argmax(actions(s))(πp(s, _))

}


object Policy {

  /**
    * Policy as a probability.
    * For given state 's' it gives a probability of choosing action 'a'.
    */
  type ΠP[S, A] = mutable.Map[(S, A), P]

  def randomPolicy[S, A]()(implicit c: MdpContext[S,A]): Policy[S, A] = new Policy[S, A](
    mdpDescription = c.mdpDescription,
    πpFactory = d => createArbitraryΠP(
      states = d.states,
      actionSet = d.actions,
      chanceProvider = abs(c.random.nextGaussian())
    )
  )

  private def createArbitraryΠP[S, A](states: States[S], actionSet: Actions[S, A], chanceProvider: => Double): ΠP[S, A] = {
    val policy: ΠP[S, A] = mutable.Map[(S, A), P]()

    states.foreach { s =>
      var totalChances: Double = 0
      val actionChances = actionSet(s).map { a =>
        //assign chance for action
        val chance: Double = chanceProvider
        totalChances += chance
        chance
      }
      val actionProbabilities = actionChances.map(_ / totalChances)

      //update policy
      actionProbabilities.zip(actionSet(s)).foreach { t =>
        val a = t._2
        val probability = t._1
        policy((s, a)) = probability
      }
    }
    policy
  }
}
