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

package com.github.mlworthing.rlai.utils

import com.github.mlworthing.rlai.Randomness


object Selector {

  /**
    * Select index of 'chances' which is was selected according to it's chance.
    * @param chances A List containing chances
    * @return an index of 'chances' list which was selected
    */
  def select(chances: Iterable[Int])(implicit randomness: Randomness): Int = {
    require(chances.nonEmpty, "chances should not be empty")
    val maxLevel = chances.sum
    val level = randomness.random.nextInt(maxLevel)
    Selector.selectByLevel(chances, level)
  }

  //TODO: Name it select. Is it bug in scala that it can't be polymorphic in `chances` parameter?
  def selectDouble(chances: Iterable[Double])(implicit randomness: Randomness): Int = {
    require(chances.nonEmpty, "chances should not be empty")
    val maxLevel = chances.sum
    val level = randomness.random.nextDouble() * maxLevel
    Selector.selectByLevel(chances, level)
  }

  /**
    * Private Selector's method.
    * Selects index by chance. It's slow but works. Not safe.
    */
  def selectByLevel[N : Numeric](chances: Iterable[N], level: N): Int = {
    val numeric = implicitly[Numeric[N]]
    val accumulated = chances
      .scanLeft(numeric.zero)(numeric.plus)
      .tail

    accumulated //a scan adds extra element in front
      .zipWithIndex
      .find(t => numeric.lt(level, t._1)) //the most tricky part
      .get._2
  }
}