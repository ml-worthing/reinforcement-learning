package com.github.mlworthing.rl.bandit

import com.github.mlworthing.rl.{Agent, Environment}

import scala.collection.mutable
import scala.util.Random

case class EGreedyAgent(
                         epsilon: Double = 0.01,
                         rate: Double = 0.1,
                         stepsToLearn: Int = 100
                       ) extends Agent[Unit, Int] {


  override def solve(environment: Environment[Unit, Int]): Seq[Int] = {

    val (_, _, actions) = environment.send(None)

    val q: mutable.Map[Int, Double] = mutable.Map(actions.map(x => (x, 0d)): _*)

    (0 to stepsToLearn).foreach { step =>
      val action = {
        val explore = Random.nextDouble() < epsilon
        if (explore) {
          val randomActionIndex = Random.nextInt(actions.length)
          actions(randomActionIndex)
        }
        else {
          q.maxBy(_._2)._1
        }
      }
      val (_, reward, _) = environment.send(Some(action))
      q(action) = q(action) + rate * (reward - q(action))

      println(q)
    }
    Seq(q.maxBy(_._2)._1)
  }
}
