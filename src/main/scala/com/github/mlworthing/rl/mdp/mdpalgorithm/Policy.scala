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

import com.github.mlworthing.rl.mdp.mdpalgorithm.Policy.ΠProbability
import com.github.mlworthing.rl.utils.MathNotation._

import scala.collection.mutable

case class ImmutableGreedPolicy[S,A](π: Map[S, A])
case class ImmutablePolicy[S,A](π: Map[(S,A), P]) {
  /**
    * Compares two policies. It returns L1 norm of differences of probabilities
    */
  def diff(p: ImmutablePolicy[S,A]): Double = π.foldLeft(0.0)((acc, c) => acc + Math.abs(c._2 - p.π(c._1)))
}

/**
  * Mutable Policy.
  */
class Policy[S, A] private[Policy](πpFactory: MdpDescription[S, A] => Policy.ΠProbability[S, A])(implicit c: MdpContext[S, A]) {

  import c.mdpDescription._

  private val πProbability: ΠProbability[S, A] = πpFactory(c.mdpDescription).withDefault(sa =>
    throw new UnsupportedOperationException(s"In state [${sa._1}] an action [${sa._2}] is not valid")
  )

  /**
    * Update policy so it promotes action 'a' for a given state 's'.
    * After update `πProbability(s)` will return `a` and `πProbability(s,a)` will return probability of `1.0`
    * Such syntax available: 'policy(s) = a'
    */
//  def update(s: S, a: A): Unit = {
//    actions(s).foreach(a => πProbability((s, a)) = 0.0)
//    πProbability((s, a)) = 1.0
//  }

  /**
    * Update policy so it promotes action 'a' for a given state 's'.
    */
  def update(s: S, a: A, promotionFactor: Double = 1.2): Unit = {
    val promotionBias: Double = 0.01
    //increase the chance of (s,a)
    val x = πProbability(s, a)
    πProbability((s,a)) = x * promotionFactor + promotionBias
    normalize(s)
  }

  //normalise, so they sum up to 1.0 again
  private def normalize(s: S): Unit = {
    val pSum = Σ(actions(s))(a => πProbability(s,a))
    actions(s).foreach (a =>πProbability((s,a)) = πProbability((s,a))/pSum)
  }

  /**
    * Returns a probability of selecting action in a given state 's'
    */
  def apply(s: S, a: A): P = πProbability((s, a))

  def apply(s: S): A = greedyAction(s)

  def greedyAction(s: S): A = try {
    argmax(actions(s))(apply(s, _))
  } catch {
    case e: UnsupportedOperationException if e.getMessage == "empty.maxBy" =>
      throw new UnsupportedOperationException(s"No actions available for state [state=$s]. Is it terminal state?")
  }

  def asGreedyImmutable(): ImmutableGreedPolicy[S, A] = ImmutableGreedPolicy(states.nonTerminalStates.map(s => s -> greedyAction(s)).toMap)
  def asImmutable(): ImmutablePolicy[S, A] = ImmutablePolicy[S,A](πProbability.toMap)
}


object Policy {

  /**
    * Policy as a probability.
    * For given state 's' it gives a probability of choosing action 'a'.
    */
  type ΠProbability[S, A] = mutable.Map[(S, A), P]

  def createRandomPolicy[S, A]()(implicit c: MdpContext[S, A]): Policy[S, A] = new Policy[S, A](
    πpFactory = d => createArbitraryΠP(
      states = d.states,
      actionSet = d.actions,
      chanceProvider = abs(c.random.nextGaussian())
    )
  )

  private def createArbitraryΠP[S, A](states: States[S], actionSet: Actions[S, A], chanceProvider: => Double): ΠProbability[S, A] = {
    val policy: ΠProbability[S, A] = mutable.Map[(S, A), P]()

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

  private def createEmptyPolicy[S,A]()(implicit c: MdpContext[S, A]): Policy[S, A] = new Policy[S, A](c =>
    mutable.Map[(S, A), P](
      {
        for {
          s <- c.states.nonTerminalStates
          a <- c.actions(s)
        } yield (s, a) -> 0.0
      }.toList : _*
    ))

  def createPolicy[S, A](v: ValueFunction[S])(implicit c: MdpContext[S, A]): Policy[S, A] = {
    import c._
    import mdpDescription._
    val π = createEmptyPolicy()

    //update π so it returns action according to v
    //http://incompleteideas.net/book/RLbook2018trimmed.pdf, page 83
    states.nonTerminalStates.foreach(s =>
      π(s) = argmax(actions(s))(a =>
        Σ(ś(s, a), rewards(s, a))((ś, r) => p(ś, r, s, a) * (r + γ * v(ś)))
      )
    )
    π
  }

  def createPolicy[S, A](sa: (S, A)*)(implicit c: MdpContext[S, A]): Policy[S, A] = {
    val π = createEmptyPolicy()
    sa.foreach((sa: (S, A)) => π(sa._1) = sa._2)
    π
  }
}
