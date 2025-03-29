#include <datastructure.h>
#include <stacktrek.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

FAST_SWITCH_DECORATOR
static i64 __handle_body_lifted_4__(i64 *);
FAST_SWITCH_DECORATOR
i64 __handler_thread_stub_lifted_5___yield(i64 *, i64, i64);
static i64 __fun_lifted_6__(i64);
static i64 __run_lifted_2__(i64, i64);
static i64 __ackermann_lifted_1__(i64, i64, i64, i64);
static closure_t *run;
static closure_t *ackermann;
enum Thread { yield };

static i64 __ackermann_lifted_1__(i64 __env__, i64 m, i64 n, i64 quota) {
  return (({
    (((((i64 *)quota)[0]) == 0)
         ? (((i64 *)quota)[0] = (RAISEZ_F(0, 0, 0, ((i64)0))))
         : 0);
    ({
      (((i64 *)quota)[0] = ((((i64 *)quota)[0]) - 1));
      ((m == 0)
           ? (n + 1)
           : ((n == 0) ? (({
               __attribute__((musttail)) return (
                   (i64(*)(i64, i64, i64, i64))__ackermann_lifted_1__)(
                   (i64)0, (i64)(m - 1), (i64)1, (i64)quota);
               0;
             }))
                       : (({
                           __attribute__((musttail)) return ((i64(*)(
                               i64, i64, i64, i64))__ackermann_lifted_1__)(
                               (i64)0, (i64)(m - 1),
                               (i64)(((i64(*)(i64, i64, i64, i64))__ackermann_lifted_1__)(
                                   (i64)0, (i64)m, (i64)(n - 1), (i64)quota)),
                               (i64)quota);
                           0;
                         }))));
    });
  }));
}

static i64 __run_lifted_2__(i64 __env__, i64 total_quota) {
  return (({
    i64 acc = (i64)(({
      i64 __field_0__ = (i64)0;
      i64 *__newref__ = xmalloc(1 * sizeof(i64));
      __newref__[0] = __field_0__;
      (i64) __newref__;
    }));
    ({
      i64 storage = (i64)(({
        i64 __field_0__ = (i64)0;
        i64 *__newref__ = xmalloc(1 * sizeof(i64));
        __newref__[0] = __field_0__;
        (i64) __newref__;
      }));
      ({
        i64 total = (i64)(({
          i64 __field_0__ = (i64)total_quota;
          i64 *__newref__ = xmalloc(1 * sizeof(i64));
          __newref__[0] = __field_0__;
          (i64) __newref__;
        }));
        ({
          i64 quota_per_yield = (i64)24000*8;
          ({
            i64 work = (i64)(({
              closure_t *__c__ = xmalloc(sizeof(closure_t));
              __c__->func_pointer = (i64)__fun_lifted_6__;
              __c__->env = (i64)xmalloc(5 * sizeof(i64));
              ((i64 *)(__c__->env))[0] = (i64)acc;
              ((i64 *)(__c__->env))[1] = (i64)ackermann;
              ((i64 *)(__c__->env))[2] = (i64)quota_per_yield;
              ((i64 *)(__c__->env))[3] = (i64)storage;
              ((i64 *)(__c__->env))[4] = (i64)total;
              (i64) __c__;
            }));
            ({
              (({
                closure_t *__clo__ = (closure_t *)work;
                i64 __f__ = (i64)(__clo__->func_pointer);
                i64 __env__ = (i64)(__clo__->env);
                ((i64(*)(i64))__f__)((i64)__env__);
              }));
              ({
                (({
                  closure_t *__clo__ = (closure_t *)work;
                  i64 __f__ = (i64)(__clo__->func_pointer);
                  i64 __env__ = (i64)(__clo__->env);
                  ((i64(*)(i64))__f__)((i64)__env__);
                }));
                (((i64 *)acc)[0]);
              });
            });
          });
        });
      });
    });
  }));
}

int main(int argc, char *argv[]) {
  init_stack_pool();
  run = xmalloc(sizeof(closure_t));
  run->func_pointer = (i64)__run_lifted_2__;
  run->env = (i64)NULL;
  ackermann = xmalloc(sizeof(closure_t));
  ackermann->func_pointer = (i64)__ackermann_lifted_1__;
  ackermann->env = (i64)NULL;

  i64 __res__ = ({
    i64 total = (i64)((i64)(readInt()));
    ({
      ((i64)(printInt((
          int64_t)(((i64(*)(i64, i64))__run_lifted_2__)((i64)0, (i64)total)))));
      0;
    });
  });
  destroy_stack_pool();
  return ((int)__res__);
}
static i64 __fun_lifted_6__(i64 __env__) {
  return (({
    i64 acc = (i64)(((i64 *)__env__)[0]);
    ({
      i64 ackermann = (i64)(((i64 *)__env__)[1]);
      ({
        i64 quota_per_yield = (i64)(((i64 *)__env__)[2]);
        ({
          i64 storage = (i64)(((i64 *)__env__)[3]);
          ({
            i64 total = (i64)(((i64 *)__env__)[4]);
            (HANDLEZ(__handle_body_lifted_4__,
                    ({SINGLESHOT, __handler_thread_stub_lifted_5___yield}),
                    ((i64)acc, (i64)ackermann, (i64)quota_per_yield,
                     (i64)storage, (i64)total)));
          });
        });
      });
    });
  }));
}

FAST_SWITCH_DECORATOR
i64 __handler_thread_stub_lifted_5___yield(i64 *__env__, i64 _, i64 k) {
  return (({
    i64 acc = (i64)(((i64 *)__env__)[0]);
    ({
      i64 ackermann = (i64)(((i64 *)__env__)[1]);
      ({
        i64 quota_per_yield = (i64)(((i64 *)__env__)[2]);
        ({
          i64 storage = (i64)(((i64 *)__env__)[3]);
          ({
            i64 total = (i64)(((i64 *)__env__)[4]);
            ({
              (((i64 *)acc)[0] = ((((i64 *)acc)[0]) + 1));
              ({
                i64 node = (i64)((
                    i64)(listNode((int64_t)k, (node_t *)((i64)(listEnd())))));
                ({
                  i64 peer = (i64)(((i64 *)storage)[0]);
                  ({
                    (((i64 *)storage)[0] = node);
                    (((i64)(listIsEmpty((node_t *)peer)))
                         ? 0
                         : (((((i64 *)total)[0]) > 0) ? ({
                             (((i64 *)total)[0] =
                                  ((((i64 *)total)[0]) - quota_per_yield));
                             (FINAL_THROW(((i64)(listHead((node_t *)peer))),
                                          quota_per_yield));
                           })
                                                      : 0));
                  });
                });
              });
            });
          });
        });
      });
    });
  }));
}

FAST_SWITCH_DECORATOR
static i64 __handle_body_lifted_4__(i64 *__env__) {
  return (({
    i64 out = ({
      i64 acc = (i64)(((i64 *)__env__)[0]);
      ({
        i64 ackermann = (i64)(((i64 *)__env__)[1]);
        ({
          i64 quota_per_yield = (i64)(((i64 *)__env__)[2]);
          ({
            i64 storage = (i64)(((i64 *)__env__)[3]);
            ({
              i64 total = (i64)(((i64 *)__env__)[4]);
              ({
                i64 local_quota = (i64)(({
                  i64 __field_0__ = (i64)quota_per_yield;
                  i64 *__newref__ = xmalloc(1 * sizeof(i64));
                  __newref__[0] = __field_0__;
                  (i64) __newref__;
                }));
                (((i64(*)(i64, i64, i64, i64))__ackermann_lifted_1__)(
                    (i64)0, (i64)4, (i64)1, (i64)local_quota));
              });
            });
          });
        });
      });
    });
    __asm__("" : "+r"(out));
    out;
  }));
}

