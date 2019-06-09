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

package object rl {

  implicit class PairOps[A, B](pair: (A, B)) {
    def first: A = pair._1
    def second: B = pair._2
    def map_1[C](f1: A => C): (C, B) = (f1(pair._1), pair._2)
    def map_2[C](f2: B => C): (A, C) = (pair._1, f2(pair._2))
    def map[C, D](f1: A => C, f2: B => D): (C, D) = (f1(pair._1), f2(pair._2))
  }

}
