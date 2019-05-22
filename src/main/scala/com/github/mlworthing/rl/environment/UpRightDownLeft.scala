package com.github.mlworthing.rl.environment

trait UpRightDownLeft {

  lazy val Up = (-1, 0)
  lazy val Right = (0, 1)
  lazy val Down = (1, 0)
  lazy val Left = (0, -1)
}
