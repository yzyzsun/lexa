  $ lexa ../lexa_snippets/helloz.lx -o main --output-c &> /dev/null
  $ cat ../lexa_snippets/helloz.c
  #include <datastructure.h>
  #include <stacktrek.h>
  #include <stdbool.h>
  #include <stdint.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  
  static i64 __handle_body_lifted_3__(i64);
  i64 __handler_Hello_lifted_4___hello(i64 *, i64);
  static i64 __run_lifted_1__(i64, i64);
  static closure_t *run;
  enum Hello { hello };
  
  static i64 __run_lifted_1__(i64 __env__, i64 n) {
    return (({
      i64 s = (i64)(({
        i64 __field_0__ = (i64)42;
        i64 *__newref__ = xmalloc(1 * sizeof(i64));
        __newref__[0] = __field_0__;
        (i64) __newref__;
      }));
      (HANDLEZ(__handle_body_lifted_3__,
               ({TAIL, __handler_Hello_lifted_4___hello}), ((i64)s)));
    }));
  }
  
  int main(int argc, char *argv[]) {
    init_stack_pool();
    run = xmalloc(sizeof(closure_t));
    run->func_pointer = (i64)__run_lifted_1__;
    run->env = (i64)NULL;
  
    i64 __res__ = (((i64(*)(i64))__run_lifted_1__)((i64)0));
    destroy_stack_pool();
    return ((int)__res__);
  }
  i64 __handler_Hello_lifted_4___hello(i64 *__env__, i64 _) {
    return (({
      i64 s = (i64)(((i64 *)__env__)[0]);
      ({
        ((i64)(strPrint((char *)(({
          i64 *__s__ = (i64 *)xmalloc(8 * sizeof(char));
          strcpy((char *)__s__, "hello\n");
          __s__;
        })))));
        ({
          ((i64)(printInt((int64_t)(((i64 *)s)[0]))));
          0;
        });
      });
    }));
  }
  
  static i64 __handle_body_lifted_3__(i64 __env__) {
    return (({
      i64 s = (i64)(((i64 *)__env__)[0]);
      (RAISEZ(0, 0, hello, ()));
    }));
  }
  
