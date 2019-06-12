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

import com.github.mlworthing.problems.playingcards.Cards._
import com.github.mlworthing.problems.playingcards._
import com.github.mlworthing.rlai.utils.Cartesian

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

sealed trait BlackJackResult
case object PlayerWon extends BlackJackResult
case object DealerWon extends BlackJackResult
case object Tie extends BlackJackResult

/**
  * Simplified BlackJack game.
  * One player only and player has only two actions.
  * Dealers hits if his value is <= 16
  */
class BlackJack(random: Random) {

  type PlayersValue = Int
  type DealersValue = Int

  /**
    * Start Game. Resets all states, shuffles new deck.
    */
  def start(): (DealersValue, PlayersValue) = {
    dealer.start()
    player.start()
    deck = mutable.Queue(Cards.shuffledDeck(random): _*)
    dealer.drawCard()
    player.drawCard()
    player.drawCard()
    (dealer.value(), player.value())
  }

  /**
    * Players action.
    */
  def hit(): PlayersValue = {
    player.drawCard()
    player.value()
  }

  /**
    * Players action. This as well finishes game. The function returns final Game Result.
    */
  def stay(): (BlackJackResult, DealersValue, PlayersValue) = {
    dealerMove()
    val result =
      if
      (dealer.value() > 21 && player.value() > 21) Tie
      else if
      (dealer.value() <= 21 && player.value() > 21) DealerWon
      else if
      (dealer.value() > 21 && player.value() <= 21) PlayerWon
      else
      /*(dealer.value() <= 21 && player.value() <= 21) */
        if(dealer.value() > player.value()) DealerWon else PlayerWon

    (result, dealer.value(), player.value())
  }

  private def dealerMove(): Unit = {
      dealer.drawCard() //mandatory draw of a second card
      if(!player.isBust() && dealer.value() < player.value()) //don't draw a card if dealer wins
        while(dealer.value() <= 16) dealer.drawCard()
  }


  private var deck: mutable.Queue[Card] = _
  private val player = new GameParticipant
  private val dealer = new GameParticipant

  private class GameParticipant {
    private val onHand: ListBuffer[Card] = ListBuffer[Card]()
    private var _value: Int = 0
    def start() = {
      onHand.clear()
      _value = 0
    }
    def drawCard(): Unit = {
      onHand += deck.dequeue()
      _value = BlackJack.value(onHand)
    }
    @inline def value(): Int = _value
    def isBust(): Boolean = _value > 21
  }

}

private object BlackJack {

  def value(onHand: Iterable[Card]): Int = {
    values(onHand).reduceLeft[Int]{(acc, c) =>
      if      (acc <= 21 && c <= 21) Math.max(acc, c)
      else if (acc <= 21 && c > 21) acc
      else if (acc > 21 && c > 21) Math.min(acc, c)
      else  /*(acc > 21 && c <= 21)*/ c
    }
  }

  def values(onHand: Iterable[Card]): Iterable[Int] = Cartesian.cartesian(onHand.map(value)).map(_.sum)

  private def value(card: Card): List[Int] = card.figure match {
    case A =>   List(1, 11)
    case K =>   List(10)
    case Q =>   List(10)
    case J =>   List(10)
    case `_10` => List(10)
    case `_9` =>  List(9)
    case `_8` =>  List(8)
    case `_7` =>  List(7)
    case `_6` =>  List(6)
    case `_5` =>  List(5)
    case `_4` =>  List(4)
    case `_3` =>  List(3)
    case `_2` =>  List(2)
  }
}

