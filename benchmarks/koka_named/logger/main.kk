module main

import std/os/env

named effect logging
  fun log(i : int): int

fun countdown(i: int, r)
  if i == 0
    then i
    else
      r.log(i)
      countdown(i - 1, r)

fun libFun(x: int, r)
  countdown(x, r)

fun plugin(x: int, r)
  libFun(x * 2, r)

fun framework(addon, x: int)
  val file_log = ref(0)
  with fileLogger <- named handler
    fun log(s) 
      file_log := !file_log + s
      0
  addon(x)
  libFun(x, fileLogger)
  println(!file_log)


pub fun main()
  val n = get-args().head("").parse-int().default(5)
  val console_log = ref(0)
  with consoleLogger <- named handler
    fun log(s) 
      console_log := !console_log + s
      0
  
  fun plugginFun(x: int)
    plugin(x, consoleLogger)
 
  framework(plugginFun, n)
  println(!console_log)
