module main

import std/os/env

named effect abort
  ctl invalid_denominator(): a

fun mod(a: int, b: int, r)
  if b <= 0
    then r.invalid_denominator()
    else
      a % b

fun divide(a: int, b: int, r)
  if b == 0
    then r.invalid_denominator()
    else
      a / b


fun extendedEuclid(a: int, b: int, x, y, gcd, r)
  if b == 0
    then 
      x := 1
      y := 0
      gcd := a
    else
      val x1 = ref(0)
      val y1 = ref(0)
      extendedEuclid(b, mod(a, b, r), x1, y1, gcd, r)
      x := !y1
      y := !x1 - divide(a, b, r) * !y1


fun runExtendedEuclid(a: int, b: int, x, y, gcd)
  with r <- named handler
    ctl invalid_denominator() println("invalid denominator")
  extendedEuclid(a, b, x, y, gcd, r)


fun loop(a: int, b: int, x, y, gcd, i: int, acc: int)
  if i == 0
    then acc
    else
      runExtendedEuclid(a, b, x, y, gcd)
      loop(a, b + 1, x, y, gcd, i - 1, acc + !x + !y + !gcd)


fun run(n: int)
  val a = 660046610375530309
  val b = 540113804746346429
  val x = ref(0)
  val y = ref(0)
  val gcd = ref(0)
  loop(a, b, x, y, gcd, n, 0)


pub fun main()
  val n = get-args().head("").parse-int().default(5)
  val r = run(n)
  println(r)

