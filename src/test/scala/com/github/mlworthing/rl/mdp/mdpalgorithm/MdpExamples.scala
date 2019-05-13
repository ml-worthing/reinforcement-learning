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

    import MdpTestData.GridWorld3x3._
    implicit val c = mdpContext

    val π: Policy[State, Action] = Policy.createRandomPolicy()
    val v: ValueFunction[State] = ValueFunction.createRandomValueFunction[State, Action]()
    Mdp.evaluatePolicy(v, π)
    Mdp.iteratePolicy(v, π)

    val π1 = Mdp.iterateValue() //todo: test iterateValue

    //TODO: true demo!?
  }


}
