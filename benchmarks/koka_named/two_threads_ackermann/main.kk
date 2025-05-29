module main

import std/os/env

named effect thread
  ctl yield(): int

fun ackermann(m: int, n: int, quota: ref<_, int>, r)
  if !quota == 0
    then
      quota := r.yield()

  quota := !quota - 1
  if m == 0
    then n + 1
    else
      if n == 0
        then ackermann(m - 1, 1, quota, r)
        else ackermann(m - 1, ackermann(m, n - 1, quota, r), quota, r)


fun run(quota_each: int)
  val acc = ref(0)
  val storage = ref(Nil)
  val total = ref(400000000)

  fun work()
    with r <- named handler
      raw ctl yield() resume
        acc := !acc + 1
        val node = Cons(rcontext, Nil)
        val peer = !storage
        storage := node
        if peer.is-empty()
          then 0
          else
            if !total > 0
              then 
                total := !total - quota_each
                (peer.head()).resume()
              else 0
    val local_quota = ref(quota_each)
    ackermann(4, 1, local_quota, r)
  work()
  work()
  !acc

pub fun main()
  val quota_each = get-args().head("").parse-int().default(5)
  val res = run(quota_each)
  0
