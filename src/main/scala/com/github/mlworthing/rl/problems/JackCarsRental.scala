package com.github.mlworthing.rl.problems

import com.github.mlworthing.rl.environments.FiniteEnvironment
import com.github.mlworthing.rl.utils.{Cache, PoissonDistribution}

/**
  * Based on "Reinforcement Learning: An Introduction. Second edition"
  * by Richard S. Sutton and Andrew G. Barto, Example 4.2
  */
object JackCarsRental {

  type State = (Int, Int)
  type Action = Int

  def apply(
    maxCarsInA: Int = 20,
    maxCarsInB: Int = 20,
    maxCarsToMove: Int = 5,
    initialCarsInA: Int = 10,
    initialCarsInB: Int = 10,
    expectedReturnsPerDayInA: Int = 3,
    expectedReturnsPerDayInB: Int = 2,
    expectedRentalsPerDayInA: Int = 3,
    expectedRentalsPerDayInB: Int = 4,
    penaltyOfMovingCar: Double = 2,
    rewardOfCarRental: Double = 10): FiniteEnvironment[State, Action] =
    new FiniteEnvironment[State, Action] {

      assert(initialCarsInA <= maxCarsInA, s"initial number of cars in location A must be at most $maxCarsInA")
      assert(initialCarsInB <= maxCarsInB, s"initial number of cars in location B must be at most $maxCarsInB")

      override def description: String =
        s"""Jack Car's Rental continuous finite MDP problem.
           |Cars at location A: $initialCarsInA/$maxCarsInA. cars
           |Cars at location B: $initialCarsInB/$maxCarsInB
           |Maximum number of cars to move: $maxCarsToMove
           |Price of renting a car: $rewardOfCarRental
           |Price of moving a car:  $penaltyOfMovingCar""".stripMargin

      override val initialStates: Set[State] = Set((initialCarsInA, initialCarsInB))
      override def isTerminalState(state: (Int, Int)): Boolean = false

      val moves: Set[Action] = (-maxCarsToMove to maxCarsToMove).toSet

      override val states: Set[State] = (for (a <- 0 to maxCarsInA; b <- 0 to maxCarsInB) yield (a, b)).toSet
      override val actions: State => Set[Action] = _ => moves

      val allPossibleTransitions: Cache[(State, Action), Seq[Transition]] = Cache(calculateNextStates)

      override val transitions: State => Action => Seq[(State, Probability, Reward)] = s =>
        a => allPossibleTransitions((s, a))

      val probabilityOfRentalInA = PoissonDistribution(expectedRentalsPerDayInA)
      val probabilityOfRentalInB = PoissonDistribution(expectedRentalsPerDayInB)
      val probabilityOfReturnInA = PoissonDistribution(expectedReturnsPerDayInA)
      val probabilityOfReturnInB = PoissonDistribution(expectedReturnsPerDayInB)

      def calculateNextStates(stateAndAction: (State, Action)): Seq[Transition] = {
        val ((carsInA, carsInB), movedFromAToB) = stateAndAction
        val carsOnMorningInA = fit(0, maxCarsInA, carsInA - movedFromAToB)
        val carsOnMorningInB = fit(0, maxCarsInB, carsInB + movedFromAToB)

        for {
          rentedInA <- 0 to carsOnMorningInA
          rentedInB <- 0 to carsOnMorningInB
          returnInA <- 0 to (maxCarsInA - carsOnMorningInA)
          returnInB <- 0 to (maxCarsInB - carsOnMorningInB)
        } yield {
          val carsOnEveningInA = fit(0, maxCarsInA, carsOnMorningInA + returnInA + rentedInA)
          val carsOnEveningInB = fit(0, maxCarsInB, carsOnMorningInB + returnInB + rentedInB)

          val nextState = (carsOnEveningInA, carsOnEveningInB)
          val probability = probabilityOfRentalInA(rentedInA, carsOnMorningInA) * probabilityOfRentalInB(
            rentedInB,
            carsOnMorningInB) * probabilityOfReturnInA(returnInA, maxCarsInA - carsOnMorningInA) * probabilityOfReturnInB(
            returnInB,
            maxCarsInB - carsOnMorningInB)
          val reward = (rentedInA + rentedInB) * rewardOfCarRental - movedFromAToB * penaltyOfMovingCar

          (nextState, probability, reward)
        }

      }

      def fit(min: Int, max: Int, value: Int): Int = if (value < min) min else if (value > max) max else value
    }

}
