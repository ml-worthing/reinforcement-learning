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

package com.github.mlworthing.rlai.utils

object MathNotation {

  /**
    * The sum function.
    * This is equivalent of: {{{xs.map(f).sum}}} but more efficient
    */
  def Σ[X](xs: Iterable[X])(f: X => Double): Double = {
    xs.foldLeft(0.0)((acc, x) => acc + f(x))
  }

  /**
    * The sum function.
    * This is equivalent of: {{{
    * val tuples = for {
    *   x <- xs
    *   y <- ys
    * } yield (x,y)
    *
    *tuples.map(t => f.tupled(f)).sum
    * }}}
    * but more efficient
    */
  def Σ[X, Y](xs: Iterable[X], ys: Iterable[Y])(f: (X, Y) => Double): Double = {
    //this is not daemon of speed
    val ix = xs.iterator

    var accumulator = 0.0
    while (ix.hasNext) {
      val x = ix.next()
      val iy = ys.iterator
      while (iy.hasNext) {
        val y = iy.next()
        accumulator += f(x, y)
      }
    }
    accumulator
  }

  //TODO: missing test
  def Σ[X, Y, Z](xs: Iterable[X], ys: Iterable[Y], zs: Iterable[Z])(f: (X, Y, Z) => Double): Double = {
    //this is not daemon of speed
    val ix = xs.iterator

    var accumulator = 0.0
    while (ix.hasNext) {
      val x = ix.next()
      val iy = ys.iterator
      while (iy.hasNext) {
        val y = iy.next()
        val iz = zs.iterator
        while(iz.hasNext) {
          val z = iz.next()
          accumulator += f(x, y, z)
        }
      }
    }
    accumulator
  }

  def argmax[T](domain: Iterable[T])(f: T => Double): T = domain.maxBy(f)

  /**
    * The same as {{{
    *   xs.map(f).max
    * }}}
    * but does not allocate intermediate array
    */
  def max_[T](xs: Iterable[T])(f: T => Double): Double = xs.tail.foldLeft(f(xs.head)){ (acc, curr) =>
    val c = f(curr)
    if(c > acc) c else acc
  }
}
