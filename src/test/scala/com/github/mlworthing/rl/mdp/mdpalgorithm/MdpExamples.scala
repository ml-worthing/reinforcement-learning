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

import org.scalatest.FreeSpec

import scala.collection.mutable

class MdpExamples extends FreeSpec {

  "3x3 grid world" in {

    // 3x3 grid world, wall at 5, goal at 9, pitfall at 3
    sealed trait Action
    case object Up extends Action
    case object Down extends Action
    case object Left extends Action
    case object Right extends Action
    implicit def toSeq[T](t: T): Seq[T] = Seq(t)

    val d = MdpDescription[Int, Action](
      states = List(1, 2, 3, 4, 6, 7, 8, 9), //middle is wall, 9 and 3 are terminal states
      actions = _ => List(Up, Down, Left, Right),
      rewards = {
        case (2, Right) => List(-1.0)
        case (6, Down) => List(-1.0)
        case (6, Up) => List(1.0)
        case (8, Right) => List(1.0)
        case (_, _) => List(0.0)
      },
      p = {
        case (4, 0.0, 1, Up) => 1.0
        case (2, 0.0, 1, Right) => 1.0
        case (1, 0.0, 2, Left) => 1.0
        case (3, -1.0, 2, Right) => 1.0
        case (7, 0.0, 4, Up) => 1.0
        case (1, 0.0, 4, Down) => 1.0
        case (9, 1.0, 6, Up) => 1.0
        case (3, -1.0, 6, Down) => 1.0
        case (8, 0.0, 7, Right) => 1.0
        case (4, 0.0, 7, Down) => 1.0
        case (9, 1.0, 8, Right) => 1.0
        case (7, 0.0, 8, Left) => 1.0
        case _ => 0.0
      }
    )

    implicit val c = MdpContext(d)
    val π: Policy[Int, Action] = Policy.randomPolicy()
    val v: mutable.Map[Int, P] = Mdp.createValueFunction0()
    Mdp.evaluatePolicy(v, π)
    Mdp.iteratePolicy(v, π)

    //TODO: true demo!?
  }


}
