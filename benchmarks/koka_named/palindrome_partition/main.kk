module main

import std/os/env
import std/core

named effect abort
  ctl invalid_arg(): a

fun partition(s: string, r)
  val res = ref(0)
  backtrack(0, s.count(), vector(s), res, r);
  !res


fun is_palindrome(sv: vector<char>, start: int, end: int)
  if start >= end
    then True
    else 
      if sv.at(start) != sv.at(end)
        then False
        else is_palindrome(sv, start + 1, end - 1)


fun backtrack_loop(i: int, n: int, sv: vector<char>, index: int, res, r)
  if i >= n
    then ()
    else
      if is_palindrome(sv, index, i)
        then
          backtrack(i + 1, n, sv, res, r)
      backtrack_loop(i + 1, n, sv, index, res, r)

fun backtrack(index: int, n, sv: vector<char>, res, r)
  if index < 0 || index > n
    then r.invalid_arg()
    else
      if index == n
        then res := !res + 1
        else
          backtrack_loop(index, n, sv, index, res, r)


fun run(s: string)
  with r <- named handler
    ctl invalid_arg()
      println("invalid argument")
      0
  partition(s, r)

pub fun main()
  val res = run("aababbaaaaabababaaabbabaaaababbaabaaaa")
  println(res)
