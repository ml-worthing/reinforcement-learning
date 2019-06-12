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

package com.github.mlworthing.rlai.chapter5

import com.github.mlworthing.rlai._

import scala.util.Random
import spire.syntax.cfor.cfor

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class MonteCarloContext[S,A](
   states: States[S],
   actions: Actions[S, A],
  environment: Environment[S,A],
  γ: Double = 0.99,
  random: Random = new Random(123)
) extends AnyRef with StatesAndActions[S,A]

abstract class Environment[S,A] {
  def send(action: A): (S, R)
  def observe(): S
}

case class EpisodeStage[S,A](state: S, action: A, rewardNext: R, isFirstVisit: Boolean)


//TODO: STILL WORK IN PRESS

object MonteCarlo {

  /**
    * First Visit Monte Carlo prediction, for estimating V ~ vπ.
    * It learns state-value function for a given policy.
    * @see Chapter 5.1, page 92.
    */
  def firstVisitMC[S,A] (
                          π: Policy[S,A],
                          v: ValueFunction[S])(implicit c: MonteCarloContext[S,A]): Unit = {
    import c._

    val returns: mutable.Map[S, mutable.ListBuffer[R]] = mutable.Map[S, ListBuffer[R]]()


    val episode: IndexedSeq[EpisodeStage[S, A]] = McHelper.generateEpisode(π)

    var G: R = 0.0

    for {
      i <- reverseRange(episode)
      t = episode(i)
    }
     {
      G = γ * G + t.rewardNext
      if (!t.isFirstVisit) {
        returns(t.state) += G
      }
    }



        ???
  }

}



object McHelper {

  //TODO: pass immutable policy, or interface which can't mutate policy
  def generateEpisode[S,A](π: Policy[S,A])(implicit c: MonteCarloContext[S,A]): IndexedSeq[EpisodeStage[S, A]] = {
    import c._

    val iterator = new Iterator[EpisodeStage[S, A]] {
      private var s: S = environment.observe()
      private val visitedStates = mutable.Set[S]()

      override def hasNext: Boolean = c.states.isTerminalState(s)

      override def next(): EpisodeStage[S, A] = {

        val a: A = π.selectActionAccordingToProbability(s)
        val (nextS: S, nextReward) = c.environment.send(a)
        val firstVisit = visitedStates.add(s)
        val triplet = EpisodeStage(s, a, nextReward, firstVisit)
        s = nextS
        triplet
      }
    }

    val episode = iterator.toArray

    episode
  }

}