module main

import std/os/env
import std/core

named effect emitter
  fun emit(e : int) : ()

fun iter (f: a -> e unit, l: list<a>) : e unit {
  match l {
    Nil -> ()
    Cons(h, t) -> { f(h); iter(f, t) }
  }
}

fun enumerate(i : int)
  if i < 0
   then Nil
   else Cons(i, enumerate(i - 1))


fun runIter(l: list<int>)
  val res = ref(0)
  with r <- named handler
    fun emit(x) res := !res + x
  fun f(x: int)
    r.emit(x)
  iter(f, l)
  !res


pub fun main()
  val n = get-args().head("").parse-int().default(5)
  fun loop(i: int, l: list<int>, acc: int)
    if i == 0
      then acc
      else
        loop(i - 1, l, acc + runIter(l))
  println(loop(n, enumerate(1000), 0))
  

