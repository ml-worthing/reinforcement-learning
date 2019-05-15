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

import scala.util.Random

/**
  * Bunch of data which are almost always required.
  *
  * @param mdpDescription
  * @param random
  * @param γ Gamma, the Discount Factor for future reward
  * @tparam S State
  * @tparam A Action
  */
case class MdpContext[S, A](
                             mdpDescription: MdpDescription[S, A],
                             γ: Double = 0.99,
                             random: Random = new Random(123)
)