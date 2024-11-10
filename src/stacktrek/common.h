#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "gc.h"

int error(const char* msg) {
    fprintf(stderr, "Error: %s\n", msg);
    exit(1);
}

#define xmalloc(size) ({                \
    void *_ptr = GC_malloc(size);          \
    if (_ptr == NULL) {                 \
        exit(EXIT_FAILURE);             \
    }                                   \
    _ptr;                               \
})


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
