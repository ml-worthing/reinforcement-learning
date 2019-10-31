package com.github.mlworthing.rl.utils

import java.util.function

case class Cache[K, V](fx: K => V) {

  private val underlying =
    new java.util.concurrent.ConcurrentHashMap[K, V]()

  private val mappingFunction: function.Function[K, V] = (k: K) => fx(k)

  def apply(key: K): V = underlying.computeIfAbsent(key, mappingFunction)

}
