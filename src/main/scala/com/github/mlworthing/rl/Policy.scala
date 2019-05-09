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

package com.github.mlworthing.rl

import scala.annotation.tailrec
import scala.util.Random

/**
  * Reinforcement learning Policy API.
  * Parametrised by the State and Action type.
  * Represents the best knowledge gained in the learning process.
  */
trait Policy[State, Action] {

  /** Run this policy in the given environment and collect rewards */
  def runWith(environment: Environment[State, Action], maxIterations: Int): Double
}

//----------------
// COMMON POLICIES
//----------------

/** The best single action to take */
case class Winner[A](action: A) extends Policy[Unit, A] {

  override def runWith(environment: Environment[Unit, A], maxIterations: Int): Double =
    environment.send(action).reward
}

/** The best plan, sequence of actions to take */
case class Trajectory[S, A](actions: Seq[A]) extends Policy[S, A] {

  override def runWith(environment: Environment[S, A], maxIterations: Int): Double =
    actions.foldLeft(0d)((s, a) => s + environment.send(a).reward)
}

/** The best action to take given the current state */
case class Deterministic[S, A](policy: Map[S, A]) extends Policy[S, A] {

  override def runWith(environment: Environment[S, A], maxIterations: Int): Double = {
    @tailrec
    def evaluate(s: S, rewardSum: Double, counter: Int): Double = {
      val observation = environment.send(policy(s))
      val newRewardSum = rewardSum + observation.reward
      if (observation.isTerminal || counter > maxIterations) newRewardSum
      else evaluate(observation.state, newRewardSum, counter + 1)
    }
    evaluate(environment.initial._1, 0d, 0)
  }
}

/** The set of alternative actions to take randomly given the current state */
case class Probabilistic[S, A](policy: Map[S, Set[(A, Double)]]) extends Policy[S, A] {

  lazy val mapStateToSortedListOfActionsWithUpperBounds: Map[S, List[(A, Double)]] = {
    policy.mapValues(set => {
      val weightSum = set.map(_._2).sum
      val list = set.toSeq
        .sortBy(pair => -pair._2)
        .map { case (action, weight) => (action, weight / weightSum) }
        .foldLeft(List.empty[(A, Double)])((list, pair) =>
          list match {
            case Nil       => pair :: list
            case head :: _ => (pair._1, pair._2 + head._2) :: list
        })
        .reverse
      list
    })
  }

  override def runWith(environment: Environment[S, A], maxIterations: Int): Double = {
    @tailrec
    def evaluate(s: S, rewardSum: Double, counter: Int): Double = {
      val random = Random.nextDouble()
      val actionSet = mapStateToSortedListOfActionsWithUpperBounds(s)
      val action = actionSet.find { case (_, limit) => random <= limit }.getOrElse(actionSet.head)._1
      val observation = environment.send(action)
      val newRewardSum = rewardSum + observation.reward
      if (observation.isTerminal || counter > maxIterations) newRewardSum
      else evaluate(observation.state, newRewardSum, counter + 1)
    }
    evaluate(environment.initial._1, 0d, 0)
  }
}
