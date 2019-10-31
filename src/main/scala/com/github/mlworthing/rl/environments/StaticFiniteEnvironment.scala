package com.github.mlworthing.rl.environments

/**
  * Special case of finite environment where its dynamics are given by
  * a static set of transitions (complete and static transition graph).
  */
trait StaticFiniteEnvironment[State, Action] extends FiniteEnvironment[State, Action] {

  type TransitionGraph = Map[State, Map[Action, Seq[Transition]]]
  val transitionGraph: TransitionGraph

  override lazy val states: Set[State] = transitionGraph.keySet
  override lazy val actions: State => Set[Action] = transitionGraph(_).keySet
  override lazy val transitions: State => Action => Seq[(State, Probability, Reward)] = transitionGraph

}
