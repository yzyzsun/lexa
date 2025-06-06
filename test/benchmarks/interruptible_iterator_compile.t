  $ lexa ../../benchmarks/lexa/interruptible_iterator/main.lx -o main &> /dev/null
  $ cat ../../benchmarks/lexa/interruptible_iterator/main.c
  #include <datastructure.h>
  #include <stacktrek.h>
  #include <stdbool.h>
  #include <stdint.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  
  static i64 __handle_body_lifted_8__(i64 *, i64 *);
  i64 __tail__handler_behead_stub_lifted_9___behead(i64 *);
  static i64 __handle_body_lifted_10__(i64 *, i64 *);
  i64 __tail__handler_replace_stub_lifted_11___replace(i64 *, i64);
  static i64 __handle_body_lifted_12__(i64 *, i64 *);
  i64 __tail__handler_yield_stub_lifted_13___yield(i64 *);
  static i64 __handle_body_lifted_14__(i64 *, i64 *);
  i64 __tail__handler_behead_main_stub_lifted_15___behead(i64 *);
  static i64 __repeat_lifted_6__(i64, i64);
  static i64 __step_lifted_5__(i64, i64, i64, i64);
  static i64 __run_lifted_4__(i64, i64);
  static i64 __yield_fun_lifted_3__(i64, i64, i64, i64);
  static i64 __listSum_lifted_2__(i64, i64, i64);
  static i64 __loop_lifted_1__(i64, i64, i64, i64);
  static closure_t *repeat;
  static closure_t *step;
  static closure_t *run;
  static closure_t *yield_fun;
  static closure_t *listSum;
  static closure_t *loop;
  enum Yield { yield };
  
  enum Replace { replace };
  
  enum Behead { behead };
  
  static i64 __loop_lifted_1__(i64 __env__, i64 it, i64 yield_stub,
                               i64 behead_stub) {
    return (({
      (HANDLE(__handle_body_lifted_10__,
              ({TAIL, __tail__handler_replace_stub_lifted_11___replace}),
              ((i64)behead_stub, (i64)it, (i64)yield_stub),
              "1_0_0_FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"));
      ({
        i64 it_tail = (i64)((i64)(listTail((node_t *)it)));
        ({
          i64 beheaded = (i64)(({
            i64 __field_0__ = (i64)0;
            i64 *__newref__ = xmalloc(1 * sizeof(i64));
            __newref__[0] = __field_0__;
            (i64) __newref__;
          }));
          ({
            i64 newtl =
                (i64)(((i64)(listIsEmpty((node_t *)it_tail)))
                          ? ((i64)(listEnd()))
                          : (HANDLE(
                                __handle_body_lifted_8__,
                                ({TAIL,
                                  __tail__handler_behead_stub_lifted_9___behead}),
                                ((i64)beheaded, (i64)it_tail, (i64)loop,
                                 (i64)yield_stub),
                                "1_0_0_FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")));
            ({
              i64 tobehead = (i64)(((i64 *)beheaded)[0]);
              ({
                i64 _ = (i64)(tobehead ? ({
                  i64 tailtail = (i64)((i64)(listTail((node_t *)newtl)));
                  ((i64)(listSetTail((node_t *)it, (node_t *)tailtail)));
                })
                                       : 0);
                it;
              });
            });
          });
        });
      });
    }));
  }
  
  static i64 __listSum_lifted_2__(i64 __env__, i64 l, i64 acc) {
    return ((((i64)(listIsEmpty((node_t *)l)))
                 ? acc
                 : (((i64(*)(i64, i64, i64))__listSum_lifted_2__)(
                       (i64)0, (i64)((i64)(listTail((node_t *)l))),
                       (i64)(acc + ((i64)(listHead((node_t *)l))))))));
  }
  
  static i64 __yield_fun_lifted_3__(i64 __env__, i64 x, i64 behead_stub,
                                    i64 replace_stub) {
    return (((x < 0) ? (RAISE(behead_stub, behead, ()))
                     : (RAISE(replace_stub, replace, ((i64)(x * 2))))));
  }
  
  static i64 __run_lifted_4__(i64 __env__, i64 n) {
    return (({
      i64 l = (i64)((i64)(listRange((int64_t)(0 - n), (int64_t)n)));
      ({
        i64 beheaded = (i64)(({
          i64 __field_0__ = (i64)0;
          i64 *__newref__ = xmalloc(1 * sizeof(i64));
          __newref__[0] = __field_0__;
          (i64) __newref__;
        }));
        ({
          i64 newtl = (i64)(HANDLE(
              __handle_body_lifted_14__,
              ({TAIL, __tail__handler_behead_main_stub_lifted_15___behead}),
              ((i64)beheaded, (i64)l, (i64)loop, (i64)yield_fun),
              "1_0_0_FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"));
          ({
            i64 tobehead = (i64)(((i64 *)beheaded)[0]);
            ({
              i64 res =
                  (i64)(tobehead ? ((i64)(listTail((node_t *)newtl))) : newtl);
              (((i64(*)(i64, i64, i64))__listSum_lifted_2__)((i64)0, (i64)res,
                                                             (i64)0));
            });
          });
        });
      });
    }));
  }
  
  static i64 __step_lifted_5__(i64 __env__, i64 i, i64 acc, i64 n_jobs) {
    return (((i == 0) ? acc
                      : (((i64(*)(i64, i64, i64, i64))__step_lifted_5__)(
                            (i64)0, (i64)(i - 1),
                            (i64)(acc + (((i64(*)(i64, i64))__run_lifted_4__)(
                                            (i64)0, (i64)n_jobs))),
                            (i64)n_jobs))));
  }
  
  static i64 __repeat_lifted_6__(i64 __env__, i64 n_jobs) {
    return ((((i64(*)(i64, i64, i64, i64))__step_lifted_5__)(
        (i64)0, (i64)1000, (i64)0, (i64)n_jobs)));
  }
  
  int main(int argc, char *argv[]) {
    init_stack_pool();
    repeat = xmalloc(sizeof(closure_t));
    repeat->func_pointer = (i64)__repeat_lifted_6__;
    repeat->env = (i64)NULL;
    step = xmalloc(sizeof(closure_t));
    step->func_pointer = (i64)__step_lifted_5__;
    step->env = (i64)NULL;
    run = xmalloc(sizeof(closure_t));
    run->func_pointer = (i64)__run_lifted_4__;
    run->env = (i64)NULL;
    yield_fun = xmalloc(sizeof(closure_t));
    yield_fun->func_pointer = (i64)__yield_fun_lifted_3__;
    yield_fun->env = (i64)NULL;
    listSum = xmalloc(sizeof(closure_t));
    listSum->func_pointer = (i64)__listSum_lifted_2__;
    listSum->env = (i64)NULL;
    loop = xmalloc(sizeof(closure_t));
    loop->func_pointer = (i64)__loop_lifted_1__;
    loop->env = (i64)NULL;
  
    i64 __res__ = ({
      i64 arg1 = (i64)((i64)(readInt()));
      ({
        i64 arg2 =
            (i64)(((i64(*)(i64, i64))__repeat_lifted_6__)((i64)0, (i64)arg1));
        ({
          ((i64)(printInt((int64_t)arg2)));
          0;
        });
      });
    });
    destroy_stack_pool();
    return ((int)__res__);
  }
  i64 __tail__handler_behead_main_stub_lifted_15___behead(i64 *__env__) {
    return (({
      i64 beheaded = (i64)(((i64 *)__env__)[0]);
      ({
        i64 l = (i64)(((i64 *)__env__)[1]);
        ({
          i64 loop = (i64)(((i64 *)__env__)[2]);
          ({
            i64 yield_fun = (i64)(((i64 *)__env__)[3]);
            (((i64 *)beheaded)[0] = 1);
          });
        });
      });
    }));
  }
  
  static i64 __handle_body_lifted_14__(i64 *__env__, i64 *behead_main_stub) {
    return (({
      i64 beheaded = (i64)(((i64 *)__env__)[0]);
      ({
        i64 l = (i64)(((i64 *)__env__)[1]);
        ({
          i64 loop = (i64)(((i64 *)__env__)[2]);
          ({
            i64 yield_fun = (i64)(((i64 *)__env__)[3]);
            (HANDLE(__handle_body_lifted_12__,
                    ({TAIL, __tail__handler_yield_stub_lifted_13___yield}),
                    ((i64)behead_main_stub, (i64)l, (i64)loop, (i64)yield_fun),
                    "1_0_0_FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"));
          });
        });
      });
    }));
  }
  
  i64 __tail__handler_yield_stub_lifted_13___yield(i64 *__env__) {
    return (({
      i64 behead_main_stub = (i64)(((i64 *)__env__)[0]);
      ({
        i64 l = (i64)(((i64 *)__env__)[1]);
        ({
          i64 loop = (i64)(((i64 *)__env__)[2]);
          ({
            i64 yield_fun = (i64)(((i64 *)__env__)[3]);
            yield_fun;
          });
        });
      });
    }));
  }
  
  static i64 __handle_body_lifted_12__(i64 *__env__, i64 *yield_stub) {
    return (({
      i64 behead_main_stub = (i64)(((i64 *)__env__)[0]);
      ({
        i64 l = (i64)(((i64 *)__env__)[1]);
        ({
          i64 loop = (i64)(((i64 *)__env__)[2]);
          ({
            i64 yield_fun = (i64)(((i64 *)__env__)[3]);
            (((i64(*)(i64, i64, i64, i64))__loop_lifted_1__)(
                (i64)0, (i64)l, (i64)yield_stub, (i64)behead_main_stub));
          });
        });
      });
    }));
  }
  
  i64 __tail__handler_replace_stub_lifted_11___replace(i64 *__env__, i64 x) {
    return (({
      i64 behead_stub = (i64)(((i64 *)__env__)[0]);
      ({
        i64 it = (i64)(((i64 *)__env__)[1]);
        ({
          i64 yield_stub = (i64)(((i64 *)__env__)[2]);
          ({
            ((i64)(listSetHead((node_t *)it, (int64_t)x)));
            0;
          });
        });
      });
    }));
  }
  
  static i64 __handle_body_lifted_10__(i64 *__env__, i64 *replace_stub) {
    return (({
      i64 behead_stub = (i64)(((i64 *)__env__)[0]);
      ({
        i64 it = (i64)(((i64 *)__env__)[1]);
        ({
          i64 yield_stub = (i64)(((i64 *)__env__)[2]);
          ({
            i64 v = (i64)((i64)(listHead((node_t *)it)));
            (({
              closure_t *__clo__ = (closure_t *)(RAISE(yield_stub, yield, ()));
              i64 __f__ = (i64)(__clo__->func_pointer);
              i64 __env__ = (i64)(__clo__->env);
              ((i64(*)(i64, i64, i64, i64))__f__)(
                  (i64)__env__, (i64)v, (i64)behead_stub, (i64)replace_stub);
            }));
          });
        });
      });
    }));
  }
  
  i64 __tail__handler_behead_stub_lifted_9___behead(i64 *__env__) {
    return (({
      i64 beheaded = (i64)(((i64 *)__env__)[0]);
      ({
        i64 it_tail = (i64)(((i64 *)__env__)[1]);
        ({
          i64 loop = (i64)(((i64 *)__env__)[2]);
          ({
            i64 yield_stub = (i64)(((i64 *)__env__)[3]);
            (((i64 *)beheaded)[0] = 1);
          });
        });
      });
    }));
  }
  
  static i64 __handle_body_lifted_8__(i64 *__env__, i64 *behead_stub) {
    return (({
      i64 beheaded = (i64)(((i64 *)__env__)[0]);
      ({
        i64 it_tail = (i64)(((i64 *)__env__)[1]);
        ({
          i64 loop = (i64)(((i64 *)__env__)[2]);
          ({
            i64 yield_stub = (i64)(((i64 *)__env__)[3]);
            (((i64(*)(i64, i64, i64, i64))__loop_lifted_1__)(
                (i64)0, (i64)it_tail, (i64)yield_stub, (i64)behead_stub));
          });
        });
      });
    }));
  }
  
