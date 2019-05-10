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

import com.github.mlworthing.rl.utils.MathNotation._

/**
  * This is the description of problem which can be solved using MDP (Markov Decision Process)
  *
  * @param p The probability of receiving state S and reward R given agent in state S and chosen action A.
  *          The dynamics of the MDP. See equation (3.2), page 48.
  * @tparam S State
  * @tparam A Action
  */
case class MdpDescription[S, A](
                                 states: States[S],
                                 actions: Actions[S, A],
                                 rewards: Rewards[S, A, R],
                                 p: (S, R, S, A) => P
                               ) {

  /**
    * Probability of transition to state 'ś' after selection of (state,action) pair
    */
  def transitionP(ś: S, s: S, a: A): P = Σ(rewards(s, a))(r => p(ś, r, s, a))

  /**
    * Possible destinations of state for given (state,action) pair
    */
  lazy val ś: Map[(S, A), Iterable[S]] = {
    (for {
      s <- states
      a <- actions(s)
      r <- rewards(s, a)
    } yield ((s, a), states.filter(p(_, r, s, a) != 0))
      ).toMap
  }

}
