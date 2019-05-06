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

package com.github.mlworthing.rl.utils

import scala.util.Random

object Selector {

  /**
    * Select index of 'chances' which is was selected according to it's chance.
    * @param random A random used by run selection
    * @param chances A List containing chances
    * @return an index of 'chances' list which was selected
    */
  def select(random: Random)(chances: List[Int]): Int = {
    require(chances.nonEmpty, "chances should not be empty")
    val maxLevel = chances.sum
    val level = random.nextInt(maxLevel)
    Selector.selectByLevel(chances, level)
  }

  /**
    * Private Selector's method.
    * Selects index by chance. It's slow but works. Not safe.
    */
  def selectByLevel(chances: List[Int], level: Int): Int = {

    val accumulated = chances
      .scanLeft(0)(_ + _)
      .tail

    accumulated //a scan adds extra element in front
      .zipWithIndex
      .find(t => level < t._1) //the most tricky part
      .get._2
  }

}

