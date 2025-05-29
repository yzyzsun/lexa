module main

import std/os/env

named effect abort
  ctl invalid_arg(): int

fun latticepaths(x1: int, y1: int, x2: int, y2: int, xbad: int, ybad: int, r)
  if x2 < x1 || y2 < y1
    then r.invalid_arg()
    else
      if x1 == x2 || y1 == y2
        then 1
        else
          if x1 == xbad && y1 == ybad
            then 0
            else
              latticepaths(x1 + 1, y1, x2, y2, xbad, ybad, r) + latticepaths(x1, y1 + 1, x2, y2, xbad, ybad, r)

pub fun main()
  val n = get-args().head("").parse-int().default(5)
  with r <- named handler
    ctl invalid_arg() println("invalid argument")
  println(latticepaths(0, 0, n, n, n/2, n/2, r))

