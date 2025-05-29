module main

import std/os/env

named effect abort
  ctl invalid_arg(): a

fun catalan_loop(i: int, n: int, acc: int, r)
  if i == n
    then acc
    else
      catalan_loop(i + 1, n, acc + catalan(i, r) * catalan(n - 1 - i, r), r)


fun catalan(n: int, r)
  if n == 0
    then 1
    else
      if n < 0
        then r.invalid_arg()
        else
          catalan_loop(0, n, 0, r)


pub fun main()
  val n = get-args().head("").parse-int().default(5)
  with r <- named handler
    ctl invalid_arg() println("invalid argument")
  println(catalan(n, r))
