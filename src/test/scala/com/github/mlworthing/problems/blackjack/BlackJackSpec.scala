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
import com.github.mlworthing.rlai.utils.UnitSpec

import scala.util.Random

class BlackJackSpec extends UnitSpec {

  "game" in {
    val blackJack = new BlackJack(new Random(123))
    new {
      //game 1
      val (dealersValue, playersValue) = blackJack.start()
      (dealersValue, playersValue) shouldBe (5,12)
      val playersValue1 = blackJack.hit()
      playersValue1 shouldBe 21
      blackJack.stay() shouldBe (PlayerWon, 25, 21)
    }

    new {
      //game 2
      val (dealersValue, playersValue) = blackJack.start()
      (dealersValue, playersValue) shouldBe (3,14)
      val playersValue1 = blackJack.hit()
      playersValue1 shouldBe 24
      blackJack.stay() shouldBe (DealerWon,13,24)
    }

    new {
      //game 3
      val (dealersValue, playersValue) = blackJack.start()
      (dealersValue, playersValue) shouldBe (6,21)
      val playersValue1 = blackJack.hit()
      playersValue1 shouldBe 18
      blackJack.stay() shouldBe (DealerWon,19,18)
    }

    new {
      //game 4
      val (dealersValue, playersValue) = blackJack.start()
      (dealersValue, playersValue) shouldBe (5,15)
      blackJack.stay() shouldBe (DealerWon,18,15)
    }

    new {
      //game 5
      val (dealersValue, playersValue) = blackJack.start()
      (dealersValue, playersValue) shouldBe (10,20)
      blackJack.stay() shouldBe (PlayerWon,19,20)
    }

    new {
      //game 6
      val (dealersValue, playersValue) = blackJack.start()
      (dealersValue, playersValue) shouldBe (10,15)
      val playersValue1 = blackJack.hit()
      playersValue1 shouldBe 25
      blackJack.stay() shouldBe (DealerWon,13,25)
    }

    new {
      //game 7
      val (dealersValue, playersValue) = blackJack.start()
      (dealersValue, playersValue) shouldBe (9,11)
      val playersValue1 = blackJack.hit()
      playersValue1 shouldBe 18
      blackJack.stay() shouldBe (DealerWon,19,18)
    }

    new {
      //game 8
      val (dealersValue, playersValue) = blackJack.start()
      (dealersValue, playersValue) shouldBe (10,13)
      blackJack.stay() shouldBe (DealerWon,18,13)
    }

  }

  "value" in {

    BlackJack.value(List(`_8♠`)) shouldBe 8

    BlackJack.values(List(`A♠`)) shouldBe List(1, 11)
    BlackJack.value(List(`A♠`)) shouldBe 11

    BlackJack.values(List(`A♠`,`A♣`)) shouldBe List(2, 12, 12, 22)
    BlackJack.value(List(`A♠`,`A♣`)) shouldBe 12

    BlackJack.value(List(`A♠`,`A♣`, `A♣`)) shouldBe 13
    BlackJack.value(List(`A♠`,`A♣`, `A♣`, `K♠`)) shouldBe 13

    BlackJack.value(List(`_8♠`)) shouldBe 8
    BlackJack.value(List(`K♠`)) shouldBe 10

    BlackJack.value(List(`A♠`,`A♣`, `A♣`, `K♠`, `_8♠`)) shouldBe 21

    BlackJack.value(List(`A♠`,`A♣`, `A♣`, `K♠`, `_9♠`)) shouldBe 22
  }


}
