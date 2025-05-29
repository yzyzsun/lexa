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

fun numdigits(a: int)
  if a < 2
    then 1
    else
      1 + numdigits(a/2)

fun pow2(n: int)
  if n == 0
    then 1
    else
      2 * pow2(n - 1)

fun karatsuba(num1: int, num2: int, threshold: int, r)
  if num1 < threshold || num2 < threshold 
    then num1 * num2
    else 
      val m = max(numdigits(num1), numdigits(num2))
      val m2 = divide(m, 2, r)
      val powm2 = pow2(m2)

      val high1 = divide(num1, powm2, r)
      val low1 = mod(num1, powm2, r)
      val high2 = divide(num2, powm2, r)
      val low2 = mod(num2, powm2, r)

      val z0 = karatsuba(low1, low2, threshold, r)
      val z1 = karatsuba(low1 + high1, low2 + high2, threshold, r)
      val z2 = karatsuba(high1, high2, threshold, r)

      z2 * powm2 * powm2 + ((z1 - z2 - z0) * powm2) + z0


fun loop(n: int, i: int, acc: int, r)
    if i == 0 
      then acc
      else
        loop(n, i - 1, acc + karatsuba(n, n, 2, r), r)


fun run(n: int, i: int)
  with r <- named handler
    ctl invalid_denominator() 
      println("invalid denominator")
      0
  loop(n, i, 0, r)


pub fun main()
  val n = get-args().head("").parse-int().default(5)
  val res = run(n, 1000000)
  println(res)
