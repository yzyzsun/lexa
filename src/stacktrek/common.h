#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "gc.h"

#define TO_GC

#ifdef TO_GC
#define xmalloc(size) ({                \
    void *_ptr = GC_malloc(size);          \
    if (_ptr == NULL) {                 \
        exit(EXIT_FAILURE);             \
    }                                   \
    _ptr;                               \
})
#else
#define xmalloc(size) malloc(size)
#endif


#ifdef DEBUG
#define DEBUG_ATTRIBUTE __attribute__((noinline))
#else
#define DEBUG_ATTRIBUTE
#endif

#ifdef DEBUG
#define DEBUG_CODE(block) block
#else
#define DEBUG_CODE(block)
#endif
#define SAVE_CONTEXT(jb, cont)

#define RESTORE_CONTEXT(jb)

#define FAST_SWITCH_DECORATOR __attribute__((preserve_none))


#define i64 intptr_t

typedef enum {
    ABORT = 0,
    SINGLESHOT,
    MULTISHOT,
    TAIL,
} handler_mode_t;

typedef struct {
  handler_mode_t mode;
  void *func;
} handler_def_t;

// For single-shot handlers, the exchanger stores either the parent's sp
// or resumption's sp.
// For multi-shot handlers, since there could be multiple copies of 
// resumptions that share the same header, they need a separate
// way of storing the resumption's sp. This is done by allocating
// a resumption_t for each resumption.
// resumption_t's rsp_sp stores the resumption's sp, and its
// exchanger_ptr stores the address of the shared exchanger.
// NOTE: the resumption k that the handler receives
// is either the bottom half of header_t or resumption_t.
typedef struct {
  i64 mode;
  handler_def_t defs [2];
  i64 env [10];
  void* exchanger;
  void** exchanger_ptr;
} header_t;

typedef struct {
    void* rsp_sp;
    void** exchanger_ptr;
} resumption_t;
