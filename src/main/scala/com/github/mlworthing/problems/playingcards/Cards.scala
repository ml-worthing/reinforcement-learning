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

package com.github.mlworthing.problems.playingcards

import scala.util.Random

sealed trait Suit

sealed trait Figure

case class Card(suit: Suit, figure: Figure) {
  override def toString(): String = s"$figure$suit".replaceAllLiterally("_", "")
}

object Cards {

  case object ♦ extends Suit // "♦" Diamond
  case object ♠ extends Suit // "♠" Spade
  case object ♣ extends Suit // "♣" Club
  case object ♥ extends Suit // "♥" Heart
  val allSuits = List(♦, ♠, ♣, ♥)

  case object A extends Figure //Ace
  case object K extends Figure //King
  case object Q extends Figure //Queen
  case object J extends Figure //Jack
  case object _10 extends Figure //Ten
  case object _9 extends Figure //Nine
  case object _8 extends Figure //Eight
  case object _7 extends Figure //Seven
  case object _6 extends Figure //Six
  case object _5 extends Figure //Five
  case object _4 extends Figure //Four
  case object _3 extends Figure //Free
  case object _2 extends Figure //Two
  val allFigures = List(A, K, Q, J, _10, _9, _8, _7, _6, _5, _4, _3, _2)

  //simple deck, no jokers
  val deck: List[Card] = for {
    suit <- allSuits
    figure <- allFigures
  } yield Card(suit, figure)

  def shuffledDeck(random: Random): List[Card] = random.shuffle(deck)

  val `A♦` = Card(♦, A)
  val `A♠` = Card(♠, A)
  val `A♣` = Card(♣, A)
  val `A♥` = Card(♥, A)

  val `K♦` = Card(♦, K)
  val `K♠` = Card(♠, K)
  val `K♣` = Card(♣, K)
  val `K♥` = Card(♥, K)

  val `Q♦` = Card(♦, Q)
  val `Q♠` = Card(♠, Q)
  val `Q♣` = Card(♣, Q)
  val `Q♥` = Card(♥, Q)

  val `J♦` = Card(♦, J)
  val `J♠` = Card(♠, J)
  val `J♣` = Card(♣, J)
  val `J♥` = Card(♥, J)

  val `_10♦` = Card(♦, _10)
  val `_10♠` = Card(♠, _10)
  val `_10♣` = Card(♣, _10)
  val `_10♥` = Card(♥, _10)

  val `_9♦` = Card(♦, _9)
  val `_9♠` = Card(♠, _9)
  val `_9♣` = Card(♣, _9)
  val `_9♥` = Card(♥, _9)

  val `_8♦` = Card(♦, _8)
  val `_8♠` = Card(♠, _8)
  val `_8♣` = Card(♣, _8)
  val `_8♥` = Card(♥, _8)

  val `_7♦` = Card(♦, _7)
  val `_7♠` = Card(♠, _7)
  val `_7♣` = Card(♣, _7)
  val `_7♥` = Card(♥, _7)

  val `_6♦` = Card(♦, _6)
  val `_6♠` = Card(♠, _6)
  val `_6♣` = Card(♣, _6)
  val `_6♥` = Card(♥, _6)

  val `_5♦` = Card(♦, _5)
  val `_5♠` = Card(♠, _5)
  val `_5♣` = Card(♣, _5)
  val `_5♥` = Card(♥, _5)

  val `_4♦` = Card(♦, _4)
  val `_4♠` = Card(♠, _4)
  val `_4♣` = Card(♣, _4)
  val `_4♥` = Card(♥, _4)

  val `_3♦` = Card(♦, _3)
  val `_3♠` = Card(♠, _3)
  val `_3♣` = Card(♣, _3)
  val `_3♥` = Card(♥, _3)

  val `_2♦` = Card(♦, _2)
  val `_2♠` = Card(♠, _2)
  val `_2♣` = Card(♣, _2)
  val `_2♥` = Card(♥, _2)

}






