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

import collection.mutable.{Map => MMap}
import scala.util.Random

/**
  * Sequence of keys with mutable weights and boost function.
  * Use to select random item according to the transformed weight.
  * @param keys sequence of unique keys
  * @param boost weight boost function, default is identity, other choice might be square or exp as well.
  * @param initialWeight initial weight value
  * @param minimalWeight minimal value of weight, if lower weight passed then all weights will shift
  * @tparam K key
  */
final class SeqWithMutableWeights[K](
  val keys: Seq[K],
  boost: Double => Double = identity,
  initialWeight: Double = 1d,
  minimalWeight: Double = 0d) {

  assert(keys.nonEmpty)

  private lazy val weights: MMap[K, Double] = MMap().withDefaultValue(Math.max(initialWeight, minimalWeight))
  private var total: Double = keys.map(k => boost(weights(k))).sum
  private var maxKey: K = keys.head

  def apply(key: K): Double = weights(key)

  def update(key: K, weight: Double): Unit =
    if (weight < minimalWeight) {
      total = 0d
      for (k <- keys if k != key) {
        weights(k) = weights(k) - weight //shift weights to keep minimal value
        total = total + boost(weights(k))
      }
      weights(key) = minimalWeight
      total = total + boost(minimalWeight)
    } else {
      weights(key) = weight
      maxKey = if (boost(weights(maxKey)) < boost(weight)) key else maxKey
      total = 0d
      for (k <- keys) {
        total = total + boost(weights(k))
      }
    }

  /**
    * Probability of taking a key given current weights and boost function.
    */
  def probabilityOf(key: K): Double = if (total == 0) 0 else boost(weights(key)) / total

  /**
    * Map of keys with respective probabilities.
    */
  def probabilities: Map[K, Double] = keys.map(k => (k, probabilityOf(k))).toMap

  /**
    * Selects random key according to the weights and boost function.
    */
  def selectRandom: K = {
    val random: Double = Random.nextDouble() * total
    var i = 0
    var level = boost(weights(keys.head))
    while (random > level) {
      i = i + 1
      if (i == keys.size) {
        print(random, level, total, weights)
      }
      level = level + boost(weights(keys(i)))
    }
    if (i == -1) print(random, level, total, weights)
    keys(i)
  }

  /**
    * Selects a key with the highest weight.
    */
  def max: K = maxKey

  def size: Int = keys.size

  override def toString(): String = weights.map { case (k, v) => s"$k->$v" }.mkString(",")

}
