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

import scala.collection.mutable

class ValueFunction[S](private[ValueFunction] val v: mutable.Map[S, R]) extends (S => R) {

  override def apply(s: S): R = v(s)

  /**
    * Synchronised update so it's thread safe.
    *
    * {{{
    *   val v: ValueFunction[S,A] = ...
    *   v(s1) = 120.5
    * }}}
    */
  def update(s: S, r: R) = v.synchronized(v.update(s, r))

  override def hashCode(): Int = v.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: ValueFunction[S] => other.v == v
    case _ => false
  }

  override def toString(): String = "<ValueFunction>"


}


object ValueFunction {
  /**
    * Create a value-function which results in zeros for terminal states and values provided by `initializer`
    * for non-terminal states
    */
  def createValueFunction[S, A](initializer: S => R)(implicit c: MdpContext[S, A]): ValueFunction[S] = {
    import c.mdpDescription._
    val vBuilder = mutable.Map.newBuilder[S, P]
    vBuilder ++= states.terminalStates.map(s => (s, 0.0))
    vBuilder ++= states.nonTerminalStates.map(s => (s, initializer(s)))
    new ValueFunction[S](vBuilder.result())
  }

  def createRandomValueFunction[S, A](scale: Double = 1.0)(implicit c: MdpContext[S, A]): ValueFunction[S] = {
    createValueFunction(_ => c.random.nextGaussian() * scale)
  }

}