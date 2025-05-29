module main

import std/os/env

named effect abort
  ctl invalid_arg(): a

fun ackermann(m: int, n: int, r)
  if m < 0 || n < 0
    then r.invalid_arg()
    else
      if m == 0
        then n + 1
        else
          if n == 0
            then ackermann(m - 1, 1, r)
            else ackermann(m - 1, ackermann(m, n - 1, r), r)

pub fun main()
  val m = get-args().head("").parse-int().default(5)
  val n = 1
  with r <- named handler
    ctl invalid_arg() println("invalid argument")
  println(ackermann(m, n, r))

