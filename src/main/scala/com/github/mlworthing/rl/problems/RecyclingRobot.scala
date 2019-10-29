package com.github.mlworthing.rl.problems

import com.github.mlworthing.rl.environments.FiniteEnvironment

/**
  * Based on "Reinforcement Learning: An Introduction. Second edition"
  * by Richard S. Sutton and Andrew G. Barto, Example 3.3
  * The recycling robot example was inspired by the can-collecting robot
  * built by Jonathan Connell (1989).
  */
object RecyclingRobot {

  sealed trait State
  case object LowEnergy extends State
  case object HighEnergy extends State

  sealed trait Action
  case object Search extends Action
  case object Wait extends Action
  case object Recharge extends Action

  def apply(alpha: Double, beta: Double, rewardSearch: Double, rewardWait: Double): FiniteEnvironment[State, Action] =
    new FiniteEnvironment[State, Action] {

      override def description: String =
        "The mobile robot having 2 states (low,high energy) and 3 possible actions (search, wait, recharge)."

      override val initialStates: Seq[State] = Seq(HighEnergy, LowEnergy)
      override val terminalStates: Set[State] = Set()

      override val transitionGraph: TransitionGraph = Map(
        LowEnergy -> Map(
          Search   -> Seq((LowEnergy, beta, rewardSearch), (HighEnergy, 1 - beta, -3)),
          Recharge -> Seq((HighEnergy, 1, 0)),
          Wait     -> Seq((LowEnergy, 1, rewardWait))
        ),
        HighEnergy -> Map(
          Search -> Seq((HighEnergy, alpha, rewardSearch), (LowEnergy, 1 - alpha, rewardSearch)),
          Wait   -> Seq((HighEnergy, 1, rewardWait))
        )
      )
    }

}
