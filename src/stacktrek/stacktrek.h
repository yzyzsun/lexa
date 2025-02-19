#include <stdio.h>
#include <stdlib.h>
#include <stack_pool.h>
#include <common.h>

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
  handler_def_t defs [2];
  i64 env [10];
  void* exchanger;
  void** exchanger_ptr;
} header_t;

typedef struct {
    void* rsp_sp;
    void** exchanger_ptr;
} resumption_t;

#define ARG_N(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, N, ...) N
#define NARGS(...) ARG_N(_, ## __VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)

__attribute__((noinline, preserve_none))
i64 RAISE_M(i64 env, i64 arg, i64 exc, i64 func) {
    resumption_t* k = (resumption_t*)xmalloc(sizeof(resumption_t));
    k->exchanger_ptr = (void**)exc;
    i64 dummyreg;
    __asm__ __volatile__ (
        "popq %[temp]\n\t" // Pop the dummy slot that is used to align the stack. It is inserted by the compiler, and is needed because of the malloc call. We pop it off because we want to expose the return address
        "movq 0(%[exc]), %[temp]\n\t" // Get the context stack from the exchanger
        "movq %%rsp, 0(%[exc])\n\t" // Save the current stack pointer to the exchanger. Later when switching back, just need to run a ret
        "movq %%rsp, 0(%[k])\n\t" // Save the current stack pointer to the resumption
        "movq %[temp], %%rsp\n\t" // Switch to the context stack
        "jmpq *%[func]\n\t" // Call the handler, the first three arguments are already in the right registers
        : [temp]"=&r"(dummyreg)
        : "D"(env), "S"(arg), [k]"d"(k), [func]"c"(func), [exc]"r"(exc)
    );
}

__attribute__((noinline, preserve_none))
i64 RAISE_M_2(i64 env, i64 arg0, i64 arg1, i64 exc, i64 func) {
    resumption_t* k = (resumption_t*)xmalloc(sizeof(resumption_t));
    k->exchanger_ptr = (void**)exc;
    i64 dummyreg;
    __asm__ __volatile__ (
        "popq %[temp]\n\t" // Pop the dummy slot that is used to align the stack. It is inserted by the compiler, and is needed because of the malloc call. We pop it off because we want to expose the return address
        "movq 0(%[exc]), %[temp]\n\t" // Get the context stack from the exchanger
        "movq %%rsp, 0(%[exc])\n\t" // Save the current stack pointer to the exchanger. Later when switching back, just need to run a ret
        "movq %%rsp, 0(%[k])\n\t" // Save the current stack pointer to the resumption
        "movq %[temp], %%rsp\n\t" // Switch to the context stack
        "jmpq *%[func]\n\t" // Call the handler, the first three arguments are already in the right registers
        : [temp]"=&r"(dummyreg)
        : "D"(env), "S"(arg0), "d"(arg1), [k]"c"(k), [func]"r"(func), [exc]"r"(exc)
    );
}

__attribute__((noinline, preserve_none))
i64 RAISE_M_3(i64 env, i64 arg0, i64 arg1, i64 arg2, i64 exc, i64 func) {
    resumption_t* k = (resumption_t*)xmalloc(sizeof(resumption_t));
    k->exchanger_ptr = (void**)exc;
    i64 dummyreg;
    __asm__ __volatile__ (
        "popq %[temp]\n\t" // Pop the dummy slot that is used to align the stack. It is inserted by the compiler, and is needed because of the malloc call. We pop it off because we want to expose the return address
        "movq 0(%[exc]), %[temp]\n\t" // Get the context stack from the exchanger
        "movq %%rsp, 0(%[exc])\n\t" // Save the current stack pointer to the exchanger. Later when switching back, just need to run a ret
        "movq %%rsp, 0(%[k])\n\t" // Save the current stack pointer to the resumption
        "movq %[temp], %%rsp\n\t" // Switch to the context stack
        "movq %[k], %%r8\n\t" // move argument to register
        "jmpq *%[func]\n\t" // Call the handler, the first three arguments are already in the right registers
        : [temp]"=&r"(dummyreg)
        : "D"(env), "S"(arg0), "d"(arg1), "c"(arg2), [k]"r"(k), [func]"r"(func), [exc]"r"(exc)
    );
}

__attribute__((noinline, preserve_none))
i64 RAISE_M_0(i64 env, i64 exc, i64 func) {
    resumption_t* k = (resumption_t*)xmalloc(sizeof(resumption_t));
    k->exchanger_ptr = (void**)exc;
    i64 dummyreg;
    __asm__ __volatile__(
        "popq %[temp]\n\t" // Pop the dummy slot that is used to align the stack. It is inserted by the compiler, and is needed because of the malloc call. We pop it off because we want to expose the return address
        "movq 0(%[exc]), %[temp]\n\t" // Get the context stack from the exchanger
        "movq %%rsp, 0(%[exc])\n\t" // Save the current stack pointer to the exchanger. Later when switching back, just need to run a ret
        "movq %%rsp, 0(%[k])\n\t" // Save the current stack pointer to the resumption
        "movq %[temp], %%rsp\n\t" // Switch to the context stack
        "jmpq *%[func]\n\t" // Call the handler, the first three arguments are already in the right registers
        : [temp]"=&r"(dummyreg)
        : "D"(env), [k]"S"(k), [func]"r"(func), [exc]"r"(exc)
    );
}

__attribute__((naked, noinline, preserve_none))
i64 RAISE(i64 env, i64 arg, i64 exc, i64 func) {
    __asm__ (
        "movq 0(%%rdx), %%rax\n\t" // Start to swap the context stack with the current stack
        "movq %%rsp, 0(%%rdx)\n\t" // Save the current stack pointer to exchanger. Later when switching back, just need to run a ret
        "movq %%rax, %%rsp\n\t" // Switch to the context stack
        "jmpq *%%rcx\n\t" // Call the handler, the first three arguments are already in the right registers
        ::
    );
}

__attribute__((naked, noinline, preserve_none))
i64 RAISE_2(i64 env, i64 arg0, i64 arg1, i64 exc, i64 func) {
    __asm__ (
        "movq 0(%%rcx), %%rax\n\t" // Start to swap the context stack with the current stack
        "movq %%rsp, 0(%%rcx)\n\t" // Save the current stack pointer to exchanger. Later when switching back, just need to run a ret
        "movq %%rax, %%rsp\n\t" // Switch to the context stack
        "jmpq *%%r8\n\t" // Call the handler, the first three arguments are already in the right registers
        ::
    );
}

__attribute__((naked, noinline, preserve_none))
i64 RAISE_3(i64 env, i64 arg0, i64 arg1, i64 arg2, i64 exc, i64 func) {
    __asm__ (
        "movq 0(%%r8), %%rbx\n\t" // Start to swap the context stack with the current stack
        "movq %%rsp, 0(%%r8)\n\t" // Save the current stack pointer to exchanger. Later when switching back, just need to run a ret
        "movq %%rbx, %%rsp\n\t" // Switch to the context stack
        "movq %%r8, %%rbx\n\t" // move argument to register
        "jmpq *%%r9\n\t" // Call the handler, the first three arguments are already in the right registers
        ::
    );
}

__attribute__((naked, noinline, preserve_none))
i64 RAISE_0(i64 env, i64 exc, i64 func) {
    __asm__ (
        "movq 0(%%rsi), %%rax\n\t" // Start to swap the context stack with the current stack
        "movq %%rsp, 0(%%rsi)\n\t" // Save the current stack pointer to exchanger. Later when switching back, just need to run a ret
        "movq %%rax, %%rsp\n\t" // Switch to the context stack
        "jmpq *%%rdx\n\t" // Call the handler, the first three arguments are already in the right registers
        ::
    );
}

__attribute__((noinline, preserve_none))
i64 RAISE_ABORT(i64 env, i64 arg, i64 exc, i64 func) {
    void* target_sp = *(void**)exc;
    void* curr_sp;
    __asm__ (
        "movq %%rsp, %0\n\t"
        : "=r"(curr_sp)
    );
    free_stack_on_abort(curr_sp, target_sp);
    __asm__ (
        "movq %[tsp], %%rsp\n\t"
        "jmpq *%[func]\n\t"
        :: "D"(env), "S"(arg), [tsp]"r"(target_sp), [func]"r"(func)
    ); 
}

__attribute__((noinline, preserve_none))
i64 RAISE_ABORT_2(i64 env, i64 arg0, i64 arg1, i64 exc, i64 func) {
    void* target_sp = *(void**)exc;
    void* curr_sp;
    __asm__ (
        "movq %%rsp, %0\n\t"
        : "=r"(curr_sp)
    );
    free_stack_on_abort(curr_sp, target_sp);
    __asm__ (
        "movq %[tsp], %%rsp\n\t"
        "jmpq *%[func]\n\t"
        :: "D"(env), "S"(arg0), "d"(arg1), [tsp]"r"(target_sp), [func]"r"(func)
    ); 
}

__attribute__((noinline, preserve_none))
i64 RAISE_ABORT_3(i64 env, i64 arg0, i64 arg1, i64 arg2, i64 exc, i64 func) {
    void* target_sp = *(void**)exc;
    void* curr_sp;
    __asm__ (
        "movq %%rsp, %0\n\t"
        : "=r"(curr_sp)
    );
    free_stack_on_abort(curr_sp, target_sp);
    __asm__ (
        "movq %[tsp], %%rsp\n\t"
        "jmpq *%[func]\n\t"
        :: "D"(env), "S"(arg0), "d"(arg1), "c"(arg2), [tsp]"r"(target_sp), [func]"r"(func)
    ); 
}

__attribute__((noinline, preserve_none))
i64 RAISE_ABORT_0(i64 env, i64 exc, i64 func) {
    void* target_sp = *(void**)exc;
    void* curr_sp;
    __asm__ (
        "movq %%rsp, %0\n\t"
        : "=r"(curr_sp)
    );
    free_stack_on_abort(curr_sp, target_sp);
    __asm__ (
        "movq %[tsp], %%rsp\n\t"
        "jmpq *%[func]\n\t"
        :: "D"(env), [tsp]"r"(target_sp), [func]"r"(func)
    ); 
}

__attribute__((noinline, naked, preserve_none))
i64 ENTER(i64* env, void* new_sp, void* body) {
    __asm__ (
        "lea 112(%%rsi), %%rcx\n\t" // Move the exchanger to rcx
        "movq %%rsp, 0(%%rcx)\n\t" // Save the current stack pointer to exchanger. Later when switching back, just need to run a ret
        "movq %%rsi, %%rsp\n\t" // Switch to the new stack new_sp
        // NB: why push the address of the exchanger, isn't it already on the relative address from the current rsp?
        // The reason is that all copies of stacks share the same exchanger. When stack is copied,
        // this address is also copied, helping the control to go back to the shared exchanger.
        "pushq %%rcx\n\t" // Push the exchanger to the new stack
        "pushq %%rcx\n\t" // Align the stack to 16 bytes
        "callq *%%rdx\n\t"
        "movq %%rax, %%r12\n\t" // Save the return value into a callee-saved register
        "movq %%rsp, %%rdi\n\t" // Move the current stack pointer to the first argument
        "callq free_stack\n\t" // Free the stack. NB HACK ATTENTTION!!!! This results in use-after-free in the next few instructions
        "popq %%rcx\n\t" // Pop the alignment
        "popq %%rcx\n\t" // Pop the exchanger
        "movq 0(%%rcx), %%rsp\n\t" // Restore the parent stack pointer
        "movq %%r12, %%rax\n\t" // Move the return value to the return register
        "retq\n\t"
        :
    );
}

__attribute__((noinline, naked, preserve_none))
i64 RESUME(i64 arg, void* exc, void* rsp_sp) {
    __asm__ (
        "movq %%rsp, 0(%%rsi)\n\t" // Save the current stack pointer to exchanger. Later when switching back, just need to run a ret
        "movq %%rdx, %%rsp\n\t" // Switch to the new stack rsp_sp
        "movq %%rdi, %%rax\n\t" // Move the argument(return value) to the return register
        "retq\n\t"
        :
    );
}

#define FIRST(x, ...) x
#define SECOND(x, y, ...) y
#define THIRD(x, y, z, ...) z
#define GET_FUNC(func, mode) func
#define GET_MODE(func, mode) mode
#define EXPAND(...) __VA_ARGS__
#define CONCAT(a, b) a ## b
#define CONCAT_EXPAND(a, b) CONCAT(a, b)
#define CONCAT5(a, b, c, d, e) a ## b ## c ## d ## e
#define CONCAT5_EXPAND(a, b, c, d, e) CONCAT5(a, b, c, d, e)

#define N_DEFS(...) ARG_N(_, ## __VA_ARGS__, 5, OOPS, 4, OOPS, 3, OOPS, 2, OOPS, 1, OOPS, 0)

void mark_defs_invariant(void*, size_t); // marker to mark defs array as invariant

#define HANDLE(body, m_defs, m_free_vars) \
({ \
    i64 out; \
    handler_def_t defs[] = {EXPAND m_defs}; \
    size_t n_defs = N_DEFS m_defs; \
    handler_mode_t mode = 0; \
    _Pragma("clang diagnostic push") \
    _Pragma("clang diagnostic ignored \"-Warray-bounds\"") \
    if (n_defs > 3) { \
        printf("%d handlers are not supported(currently 2 max)\n", n_defs); \
        exit(EXIT_FAILURE); \
    } \
    if (defs[0].mode == TAIL && (n_defs <= 1 || defs[1].mode == TAIL) && (n_defs <= 2 || defs[2].mode == TAIL)) { \
        mode = TAIL; \
    } else if ((defs[0].mode == SINGLESHOT || defs[0].mode == MULTISHOT) || \
                (n_defs > 1 && (defs[1].mode == SINGLESHOT || defs[1].mode == MULTISHOT)) || \
                (n_defs > 2 && (defs[2].mode == SINGLESHOT || defs[2].mode == MULTISHOT))) { \
        mode = MULTISHOT; \
    } else { \
        mode = ABORT; \
    } \
    _Pragma("clang diagnostic pop") \
    out = _HANDLE(mode, body, m_defs, m_free_vars); \
    out; \
})

// TODO: GC_set_main_stack_sp does not work with nested general handlers
#define _HANDLE(mode, body, m_defs, m_free_vars) \
    ({ \
    i64 out; \
    if (mode == TAIL) { \
        header_t stub = { \
            .defs = { EXPAND m_defs }, \
            .env = { EXPAND m_free_vars } \
        }; \
        mark_defs_invariant(&stub.defs, N_DEFS m_defs); \
        out = body((i64*)stub.env, (i64*)&stub); \
    } else if (mode == ABORT) { \
        header_t stub = { \
            .defs = { EXPAND m_defs }, \
            .env = { EXPAND m_free_vars }, \
            .exchanger_ptr = NULL \
        }; \
        stub.exchanger_ptr = &stub.exchanger; \
        long dummyreg; /* dummyreg used to allow compiler to pick available register */ \
        __asm__ __volatile__ ( \
            "annotation_marker%=_nocapture_0: \n" \
            "lea -8(%%rsp), %[temp]\n\t" \
            "movq %[temp], 0(%1)\n\t" \
            : [temp]"=&r"(dummyreg)\
            : "r"(&stub.exchanger) \
        ); \
        [[clang::noinline]]out = ((i64(*FAST_SWITCH_DECORATOR)(__attribute__((noescape)) i64 *, __attribute__((noescape)) i64 *))body)((i64*)stub.env, (i64*)&stub); \
    } else { \
        handler_def_t _defs[] = {EXPAND m_defs}; \
        i64 _env[] = {EXPAND m_free_vars}; \
        char* new_sp = get_stack(); \
        new_sp = (char*)((i64)new_sp & ~0xF); \
        new_sp -= sizeof(header_t); \
        header_t* stub = (header_t*)new_sp; \
        memcpy(&stub->defs, &_defs, sizeof(_defs)); \
        memcpy(&stub->env, &_env, sizeof(_env)); \
        stub->exchanger_ptr = &stub->exchanger; \
        GC_set_main_stack_sp(); \
        out = ENTER(stub->env, new_sp, body); \
    } \
    out; \
    })


#define HANDLEZ(body, m_defs, m_free_vars) \
({ \
    i64 out; \
    handler_def_t defs[] = {EXPAND m_defs}; \
    size_t n_defs = N_DEFS m_defs; \
    handler_mode_t mode = 0; \
    _Pragma("clang diagnostic push") \
    _Pragma("clang diagnostic ignored \"-Warray-bounds\"") \
    if (n_defs > 3) { \
        printf("%d handlers are not supported(currently 2 max)\n", n_defs); \
        exit(EXIT_FAILURE); \
    } \
    if (defs[0].mode == TAIL && (n_defs <= 1 || defs[1].mode == TAIL) && (n_defs <= 2 || defs[2].mode == TAIL)) { \
        mode = TAIL; \
    } else if ((defs[0].mode == SINGLESHOT || defs[0].mode == MULTISHOT) || \
                (n_defs > 1 && (defs[1].mode == SINGLESHOT || defs[1].mode == MULTISHOT)) || \
                (n_defs > 2 && (defs[2].mode == SINGLESHOT || defs[2].mode == MULTISHOT))) { \
        mode = MULTISHOT; \
    } else { \
        mode = ABORT; \
    } \
    _Pragma("clang diagnostic pop") \
    out = _HANDLEZ(mode, body, m_defs, m_free_vars); \
    out; \
})


// TODO: GC_set_main_stack_sp does not work with nested general handlers
#define _HANDLEZ(mode, body, m_defs, m_free_vars) \
    ({ \
    i64 out; \
    if (mode == TAIL) { \
        header_t stub = { \
            .defs = { EXPAND m_defs }, \
            .env = { EXPAND m_free_vars } \
        }; \
        i64 exchanger_ptr = (i64)&stub.exchanger; \
        i64 env = (i64)stub.env; \
        __asm__ __volatile__ ( \
            "lea -24(%%rsp), %%r10\n\t" \
            "movq %%r10, 0(%[exc])\n\t" \
            "pushq %[exc]\n\t" \
            "pushq %[exc]\n\t" \
            "callq %P[body_]\n\t" \
            "addq $16, %%rsp\n\t" \
            : "=a"(out), "+D"(env), [exc]"+d"(exchanger_ptr)\
            : [body_]"i"(body) \
            : "rsi", "rcx", "r8", "r9", "r10", "r11", \
            "rbx", "rbp", "r12", "r13", "r14", "r15", \
            "xmm0","xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", \
            "xmm8","xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15", \
            "mm0","mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm6", \
            "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)" \
        ); \
    } else if (mode == ABORT) { \
        header_t stub = { \
            .defs = { EXPAND m_defs }, \
            .env = { EXPAND m_free_vars } \
        }; \
        stub.exchanger_ptr = &stub.exchanger; \
        i64 exchanger_ptr = (i64)&stub.exchanger; \
        i64 env_ptr = (i64)stub.env; \
        __asm__ __volatile__ ( \
            "lea -24(%%rsp), %%r10\n\t" \
            "movq %%r10, 0(%[exc])\n\t" \
            "pushq %[exc]\n\t" \
            "pushq %[exc]\n\t" \
            "callq %P[body_]\n\t" \
            "addq $16, %%rsp\n\t" \
            : "=a"(out), "+D"(env_ptr), [exc]"+d"(exchanger_ptr)\
            : [body_]"i"(body) \
            : "rsi", "rcx", "r8", "r9", "r10", "r11", \
            "rbx", "rbp", "r12", "r13", "r14", "r15", \
            "xmm0","xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", \
            "xmm8","xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15", \
            "mm0","mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm6", \
            "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)" \
        ); \
    } else { \
        handler_def_t _defs[] = {EXPAND m_defs}; \
        i64 _env[] = {EXPAND m_free_vars}; \
        char* new_sp = get_stack(); \
        new_sp = (char*)((i64)new_sp & ~0xF); \
        new_sp -= sizeof(header_t); \
        header_t* stub = (header_t*)new_sp; \
        memcpy(&stub->defs, &_defs, sizeof(_defs)); \
        memcpy(&stub->env, &_env, sizeof(_env)); \
        stub->exchanger_ptr = &stub->exchanger; \
        GC_set_main_stack_sp(); \
        out = ENTER(stub->env, new_sp, body); \
    } \
    out; \
    })


static i64 (FAST_SWITCH_DECORATOR* raise_table[3])(i64 env, i64 arg, i64 exc, i64 func) = {
    RAISE_ABORT,
    RAISE,
    RAISE_M,
};

static i64 (FAST_SWITCH_DECORATOR* raise_table_2[3])(i64 env, i64 arg0, i64 arg1, i64 exc, i64 func) = {
    RAISE_ABORT_2,
    RAISE_2,
    RAISE_M_2,
};

static i64 (FAST_SWITCH_DECORATOR* raise_table_3[3])(i64 env, i64 arg0, i64 arg1, i64 arg2, i64 exc, i64 func) = {
    RAISE_ABORT_3,
    RAISE_3,
    RAISE_M_3,
};

static i64 (FAST_SWITCH_DECORATOR* raise_table_0[3])(i64 env, i64 exc, i64 func) = {
    RAISE_ABORT_0,
    RAISE_0,
    RAISE_M_0,
};

header_t* stackwalk();

#define RAISE(_stub, index, m_args) \
    ({ \
    header_t* stub = (header_t*)_stub; \
    i64 out; \
    i64 nargs = NARGS m_args; \
    i64 args[] = {EXPAND m_args}; \
    _Pragma("clang diagnostic push") \
    _Pragma("clang diagnostic ignored \"-Warray-bounds\"") \
    if (stub->defs[index].mode == TAIL) { \
        if (nargs == 0) { \
            out = ((i64(*)(i64*))stub->defs[index].func)(stub->env); \
        } else if (nargs == 1) { \
            out = ((i64(*)(i64*, i64))stub->defs[index].func)(stub->env, args[0]); \
        } else if (nargs == 2) { \
            out = ((i64(*)(i64*, i64, i64))stub->defs[index].func)(stub->env, args[0], args[1]); \
        } else if (nargs == 3) { \
            out = ((i64(*)(i64*, i64, i64, i64))stub->defs[index].func)(stub->env, args[0], args[1], args[2]); \
        } else { \
            exit(EXIT_FAILURE); \
        } \
    } else { \
        if (nargs == 1) { \
            out = raise_table[stub->defs[index].mode]((i64)stub->env, (i64)args[0], (i64)stub->exchanger_ptr, (i64)stub->defs[index].func); \
        } else if (nargs == 2) { \
            out = raise_table_2[stub->defs[index].mode]((i64)stub->env, (i64)args[0], (i64)args[1], (i64)stub->exchanger_ptr, (i64)stub->defs[index].func); \
        } else if (nargs == 3) { \
            out = raise_table_3[stub->defs[index].mode]((i64)stub->env, (i64)args[0], (i64)args[1], (i64)args[2], (i64)stub->exchanger_ptr, (i64)stub->defs[index].func); \
        } else if (nargs == 0) { \
            out = raise_table_0[stub->defs[index].mode]((i64)stub->env, (i64)stub->exchanger_ptr, (i64)stub->defs[index].func); \
        } else { \
            printf("Number of args to raise unsupported\n"); exit(EXIT_FAILURE); \
        } \
    } \
    _Pragma("clang diagnostic pop") \
    out; \
    })

#define RAISEZ(clue_sig, clue_dist, index, m_args) \
    ({ \
    header_t* stub = stackwalk(clue_sig, clue_dist); \
    i64 out; \
    i64 nargs = NARGS m_args; \
    i64 args[] = {EXPAND m_args}; \
    _Pragma("clang diagnostic push") \
    _Pragma("clang diagnostic ignored \"-Warray-bounds\"") \
    if (stub->defs[index].mode == TAIL) { \
        if (nargs == 0) { \
            out = ((i64(*)(i64*))stub->defs[index].func)(stub->env); \
        } else if (nargs == 1) { \
            i64 env = (i64)stub->env; \
            i64 arg = args[0]; \
            i64 func = (i64)stub->defs[index].func; \
            i64 exchanger_ptr = (i64)&stub->exchanger; \
            __asm__ __volatile__ ( \
                "pushq %[exc]\n\t" \
                "pushq %[exc]\n\t" \
                "callq *%[body_]\n\t" \
                "addq $16, %%rsp\n\t" \
                : "=a"(out), "+D"(env), "+S"(arg), [body_]"+r"(func), [exc]"+r"(exchanger_ptr) \
                : \
                : "rcx", "rdx", "r8", "r9", "r10", "r11", \
                "xmm0","xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", \
                "xmm8","xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15", \
                "mm0","mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm6", \
                "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)" \
            ); \
        } else if (nargs == 2) { \
            out = ((i64(*)(i64*, i64, i64))stub->defs[index].func)(stub->env, args[0], args[1]); \
        } else if (nargs == 3) { \
            out = ((i64(*)(i64*, i64, i64, i64))stub->defs[index].func)(stub->env, args[0], args[1], args[2]); \
        } else { \
            exit(EXIT_FAILURE); \
        } \
    } else { \
        if (nargs == 1) { \
            out = raise_table[stub->defs[index].mode]((i64)stub->env, (i64)args[0], (i64)stub->exchanger_ptr, (i64)stub->defs[index].func); \
        } else if (nargs == 2) { \
            out = raise_table_2[stub->defs[index].mode]((i64)stub->env, (i64)args[0], (i64)args[1], (i64)stub->exchanger_ptr, (i64)stub->defs[index].func); \
        } else if (nargs == 3) { \
            out = raise_table_3[stub->defs[index].mode]((i64)stub->env, (i64)args[0], (i64)args[1], (i64)args[2], (i64)stub->exchanger_ptr, (i64)stub->defs[index].func); \
        } else if (nargs == 0) { \
            out = raise_table_0[stub->defs[index].mode]((i64)stub->env, (i64)stub->exchanger_ptr, (i64)stub->defs[index].func); \
        } else { \
            printf("Number of args to raise unsupported\n"); exit(EXIT_FAILURE); \
        } \
    } \
    _Pragma("clang diagnostic pop") \
    out; \
    })

#define THROW(k, arg) \
    ({ \
    i64 out; \
    char* new_sp = dup_stack((char*)((resumption_t*)k)->rsp_sp); \
    GC_set_main_stack_sp(); \
    out = RESUME(arg, ((resumption_t*)k)->exchanger_ptr, new_sp); \
    out; \
    })

#define FINAL_THROW(k, arg) \
    ({ \
    i64 out; \
    GC_set_main_stack_sp(); \
    out = RESUME(arg, ((resumption_t*)k)->exchanger_ptr, ((resumption_t*)k)->rsp_sp); \
    out; \
    })

i64 mathAbs(i64 a) {
  return labs(a);
}

#define readInt() (argc == 2) ? atoi(argv[1]) : (printf("Usage: %s <int>\n", argv[0]), exit(EXIT_FAILURE), 0)
#define printInt(x) printf("%ld\n", x)

typedef struct {
  i64 func_pointer;
  i64 env;
  i64 num_fv;
} closure_t;