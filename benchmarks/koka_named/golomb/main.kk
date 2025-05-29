module main

import std/os/env

named effect abort
  ctl invalid_arg(): a

fun golomb(n: int, r)
  if n <= 0
    then r.invalid_arg()
    else
      if n == 1
        then 1
        else
          1 + golomb(n - golomb(golomb(n - 1, r), r), r)

pub fun main()
  val n = get-args().head("").parse-int().default(5)
  with r <- named handler
    ctl invalid_arg() println("invalid argument")
  println(golomb(n, r))

