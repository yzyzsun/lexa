module main

import std/os/env

named effect abort
  ctl invalid_arg(): a

fun hofstadterq(n: int, r)
  if n < 1
    then r.invalid_arg()
    else
      if n < 3
        then 1
        else
          hofstadterq(n - hofstadterq(n - 1, r), r) + hofstadterq(n - hofstadterq(n - 2, r), r)

pub fun main()
  val n = get-args().head("").parse-int().default(5)
  with r <- named handler
    ctl invalid_arg() println("invalid argument")
  println(hofstadterq(n, r))

