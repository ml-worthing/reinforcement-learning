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
