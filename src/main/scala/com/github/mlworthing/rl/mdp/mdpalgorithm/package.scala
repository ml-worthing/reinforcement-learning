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

package com.github.mlworthing.rl.mdp

package object mdpalgorithm {

  /**
    * Reward
    */
  type R = Double

  /**
    * Probability, a number from [0,1] range
    */
  type P = Double

  /**
    * Actions for given S,A
    */
  type ActionSet[S, A] = S => Iterable[A]

  /**
    * Rewards for given (state,action) pair
    */
  type RewardSet[S, A, R] = (S, A) => Iterable[R]

  /**
    * All states for particular MDP
    */
  type StateSet[S] = Iterable[S]

}
