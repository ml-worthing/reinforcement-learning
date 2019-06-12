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

package com.github.mlworthing.problems.blackjack

import com.github.mlworthing.problems.playingcards.Card

object BlackJackApp extends App {

  val bj = new BlackJack()

  var gameNo = 0
  var score = 0


  def stateString(state: BjGameState): String = {
    s"[Dealer ${state.dealersCards.mkString(",")} (${state.dealersValue}points)] " +
    s"[Player ${state.playersCards.mkString(",")} (${state.playersValue}points)] " +
    s"[TotalScore: $score] "
  }
  while (true) {
    println()
    println(s"Game $gameNo")
    val state = bj.start()
    println(s"${stateString(state)} | Enter [S]tay or [H]it")

    var cont = true
    do {

      def finishGame(state: BjGameState) = {

        state.result match {
          case Tie =>
            cont = false
            println(s"It's a TIE ${stateString(state)}")
          case DealerWon =>
            score -= 1
            cont = false
            println(s"You LOST ${stateString(state)}")
          case PlayerWon =>
            score += 1
            cont = false
            println(s"You WIN ${stateString(state)}")
        }
      }
      io.StdIn.readLine() match {
        case s if s.startsWith("H") =>
          val state = bj.hit()
          if(state.result == InProgress)
            println(s"${stateString(state)} | Enter [S]tay or [H]it")
          else
            finishGame(state)
        case s if s.startsWith("S") =>
          val state = bj.stay()
          finishGame(state)
        case _ =>
          println(s"| Enter [S]tay or [H]it")
      }

    } while (cont)
    gameNo += 1
  }


}