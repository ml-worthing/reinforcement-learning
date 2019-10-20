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

package com.github.mlworthing
package rl

import scala.annotation.tailrec
import scala.util.Random

/**
  * Reinforcement learning Policy API.
  *
  * A policy is a mapping from states to probabilities of selecting each possible action.
  *
  * Parametrised by the `State` and `Action` types.
  * Represents the best knowledge gained in the learning process.
  */
trait Policy[State, Action] {

  /** Run this policy in the given environment and collect rewards */
  def execute(environment: Environment[State, Action], maxIterations: Int): Double
}

//----------------
// COMMON POLICIES
//----------------

/** The best single action to take always */
case class Winner[A](action: A) extends Policy[Unit, A] {

  override def execute(environment: Environment[Unit, A], maxIterations: Int): Double = {
    val (_, _, frame) = environment.initial
    environment.step(action, frame).first.reward
  }
}

/** The best actions to take no matter what current state is, each with probability 1 */
case class Trajectory[S, A](actions: Seq[A]) extends Policy[S, A] {

  override def execute(environment: Environment[S, A], maxIterations: Int): Double = {
    val (_, _, frame) = environment.initial
    actions
      .foldLeft((0d, frame)) {
        case ((acc, frame), action) => environment.step(action, frame).map_1(_.reward + acc)
      }
      .first
  }
}

/** The best actions to take given the current state, each with probability 1 */
case class Deterministic[S, A](policy: Map[S, A]) extends Policy[S, A] {

  override def execute(environment: Environment[S, A], maxIterations: Int): Double = {
    val (_, _, initialFrame) = environment.initial
    @tailrec
    def execute(state: S, rewardSum: Double, counter: Int, frame: environment.Frame): Double = {
      val (observation, nextFrame) =
        if (policy.contains(state)) environment.step(policy(state), frame)
        else (environment.Observation(state, 0d, Set.empty, isTerminal = true), frame)
      val newRewardSum = rewardSum + observation.reward
      if (observation.isTerminal || counter > maxIterations) newRewardSum
      else execute(observation.state, newRewardSum, counter + 1, nextFrame)
    }
    execute(environment.initial._1, 0d, 0, initialFrame)
  }
}

/** The set of alternative actions to take given the current state with their respective probabilities */
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

  override def execute(environment: Environment[S, A], maxIterations: Int): Double = {
    val (_, _, initialFrame) = environment.initial
    @tailrec
    def execute(s: S, rewardSum: Double, counter: Int, frame: environment.Frame): Double = {
      val random = Random.nextDouble()
      val actionSet = mapStateToSortedListOfActionsWithUpperBounds(s)
      val action = actionSet.find { case (_, limit) => random <= limit }.getOrElse(actionSet.head)._1
      val (observation, nextFrame) = environment.step(action, frame)
      val newRewardSum = rewardSum + observation.reward
      if (observation.isTerminal || counter > maxIterations) newRewardSum
      else execute(observation.state, newRewardSum, counter + 1, nextFrame)
    }
    execute(environment.initial._1, 0d, 0, initialFrame)
  }
}
