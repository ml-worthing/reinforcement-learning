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
  def runWith(environment: Environment[State, Action]): Double
}

//----------------
// COMMON POLICIES
//----------------

/** The best single action to take */
case class Winner[A](action: A) extends Policy[Unit, A] {

  override def runWith(environment: Environment[Unit, A]): Double =
    environment.send(action).reward
}

/** The best plan, sequence of actions to take */
case class Trajectory[S, A](actions: Seq[A]) extends Policy[S, A] {

  override def runWith(environment: Environment[S, A]): Double =
    actions.foldLeft(0d)((s, a) => s + environment.send(a).reward)
}

/** The best action to take given the current state */
case class Deterministic[S, A](policy: Map[S, A]) extends Policy[S, A] {

  override def runWith(environment: Environment[S, A]): Double = {
    @tailrec
    def evaluate(s: S, rewardSum: Double): Double = {
      val observation = environment.send(policy(s))
      val newRewardSum = rewardSum + observation.reward
      if (observation.isTerminal) newRewardSum else evaluate(observation.state, newRewardSum)
    }
    evaluate(environment.initial._1, 0d)
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
      list
    })
  }

  override def runWith(environment: Environment[S, A]): Double = {
    @tailrec
    def evaluate(s: S, rewardSum: Double): Double = {
      val random = Random.nextDouble()
      val actionSet = mapStateToSortedListOfActionsWithUpperBounds(s)
      val action = actionSet.find { case (_, limit) => random <= limit }.getOrElse(actionSet.head)._1
      val observation = environment.send(action)
      val newRewardSum = rewardSum + observation.reward
      if (observation.isTerminal) newRewardSum else evaluate(observation.state, newRewardSum)
    }
    evaluate(environment.initial._1, 0d)
  }
}
