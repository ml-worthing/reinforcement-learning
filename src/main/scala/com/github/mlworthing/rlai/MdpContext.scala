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

import com.github.mlworthing.rlai.utils.MathNotation.Σ

import scala.util.Random

trait Randomness {
  def random: Random
}

object Randomness {
  def apply(seed: Int = 0): Randomness = new Randomness{
    override def random: Random = new Random(seed)
  }
}
/**
  * This is the description of problem which can be solved using MDP (Markov Decision Process) and
  * Bunch of data which are almost always required when solving MDP.
  *
  * @param p The probability of receiving state S and reward R given agent in state S and chosen action A.
  *          The dynamics of the MDP. See equation (3.2), page 48.
  * @param γ Gamma, the Discount Factor for future reward
  * @tparam S State
  * @tparam A Action
  */
case class MdpContext[S, A](
                             states: States[S],
                             actions: Actions[S, A],
                             rewards: Rewards[S, A, R],
                             p: (S, R, S, A) => P,
                             γ: Double = 0.99,
                             random: Random = new Random(123)
) extends AnyRef with Randomness with StatesAndActions[S,A] {

  states.nonTerminalStates.foreach{ s =>
    //sum of probabilities to reach state s` given that you were in state s and chosen action a
    val totalTransitionProbabilityTransitioning = Σ(actions(s))(a => Σ(ś(s,a))(ś => transitionP(ś, s, a)))

    require(totalTransitionProbabilityTransitioning > 0,
      s"Erroneous MdpDescription. Sum of probabilities p(ś, [s=$s], a]) was 0. There is no transition available to any state given (s,a)"
    )
  }

  /**
    * Probability of transition to state 'ś' after selection of (state,action) pair
    */
  def transitionP(ś: S, s: S, a: A): P = Σ(rewards(s, a))(r => p(ś, r, s, a))

  /**
    * Possible destinations of state for given (state,action) pair.
    * Results in map in which for each (S,A) probability of transition is non-zero.
    */
  lazy val ś: Map[(S, A), Iterable[S]] = {
    (for {
      s <- states.nonTerminalStates
      a <- actions(s)
      r <- rewards(s, a)
      destinations = states.filter(p(_, r, s, a) != 0)
      if destinations.nonEmpty
    } yield ((s, a), destinations)
      ).toMap
  }
}
