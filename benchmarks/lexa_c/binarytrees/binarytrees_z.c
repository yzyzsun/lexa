/* The Computer Language Shootout Benchmarks
   http://shootout.alioth.debian.org/

   contributed by Kevin Carson
   compilation:
       gcc -O3 -fomit-frame-pointer -funroll-loops -static binary-trees.c -lm
       icc -O3 -ip -unroll -static binary-trees.c -lm
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stacktrek.h>

i64 handler_malloc_exception(i64*);
i64 handler_null_exception(i64*);
FAST_SWITCH_DECORATOR
i64 stretched_tree_handle_body(i64*);
FAST_SWITCH_DECORATOR
i64 tree_handle_body(i64*);
FAST_SWITCH_DECORATOR
i64 long_lived_tree_create_handle_body(i64*);
FAST_SWITCH_DECORATOR
i64 long_lived_tree_test_handle_body(i64*);


typedef struct tn {
    struct tn*    left;
    struct tn*    right;
    long          item;
} treeNode_t;

i64 handler_malloc_exception(i64 *env) {
    // printf("Exception: malloc() failed to allocate memory\n");
    return 1;
}

i64 handler_null_exception(i64 *env) {
    // printf("Exception: NULL pointer\n");
    return 1;
}

treeNode_t* NewTreeNode(treeNode_t* left, treeNode_t* right, long item)
{
    treeNode_t* new;

    new = (treeNode_t*)malloc(sizeof(treeNode_t));

    if (new == NULL) {
        RAISEZ(0, 0, 0, ());
    }

    new->left = left;
    new->right = right;
    new->item = item;

    return new;
} /* NewTreeNode() */


long ItemCheck(treeNode_t* tree)
{
    if (tree == NULL) {
        RAISEZ(0, 0, 1, ());
    }

    if (tree->left == NULL)
        return tree->item;
    else
        return tree->item + ItemCheck(tree->left) - ItemCheck(tree->right);
} /* ItemCheck() */


treeNode_t* BottomUpTree(long item, unsigned depth)
{
    if (depth > 0)
        return NewTreeNode
        (
            BottomUpTree(2 * item - 1, depth - 1),
            BottomUpTree(2 * item, depth - 1),
            item
        );
    else
        return NewTreeNode(NULL, NULL, item);
} /* BottomUpTree() */


void DeleteTree(treeNode_t* tree)
{
    // if (tree == NULL) {
    //     RAISEZ(abort_stub, 1, ());
    // }

    if (tree->left != NULL)
    {
        DeleteTree(tree->left);
        DeleteTree(tree->right);
    }

    free(tree);
} /* DeleteTree() */


int main(int argc, char* argv[])
{
    unsigned   N, minDepth, maxDepth, stretchDepth;
    treeNode_t *longLivedTree;

    N = argc < 2 ? 12 : atol(argv[1]);

    minDepth = 4;

    if ((minDepth + 2) > N)
        maxDepth = minDepth + 2;
    else
        maxDepth = N;

    stretchDepth = maxDepth + 1;

    HANDLEZ(
        stretched_tree_handle_body, 
        ({ABORT, handler_malloc_exception},
        {ABORT, handler_null_exception}),
        ((i64)stretchDepth)
    );

    HANDLEZ(
        long_lived_tree_create_handle_body, 
        ({ABORT, handler_malloc_exception},
        {ABORT, handler_null_exception}),
        ((i64)&longLivedTree, (i64)maxDepth)
    );

    HANDLEZ(
        tree_handle_body,
        ({ABORT, handler_malloc_exception},
        {ABORT, handler_null_exception}),
        ((i64)minDepth, (i64)maxDepth)
    );

    HANDLEZ(
        long_lived_tree_test_handle_body, 
        ({ABORT, handler_malloc_exception},
        {ABORT, handler_null_exception}),
        ((i64)longLivedTree, (i64)maxDepth)
    );

    return 0;
} /* main() */

FAST_SWITCH_DECORATOR
i64 stretched_tree_handle_body(i64 *env) {
    treeNode_t *stretchTree;
    unsigned int stretchDepth = (unsigned int)env[0];

    stretchTree = BottomUpTree(0, stretchDepth);
    // printf
    (
        "stretch tree of depth %u\t check: %li\n",
        stretchDepth,
        ItemCheck(stretchTree)
    );

    DeleteTree(stretchTree);
    return 0;
}

FAST_SWITCH_DECORATOR
i64 tree_handle_body(i64 *env) {
    treeNode_t *tempTree;
    unsigned int minDepth = (unsigned int)env[0];
    unsigned int maxDepth = (unsigned int)env[1];
    for (int depth = minDepth; depth <= maxDepth; depth += 2)
    {
        long    i, iterations, check;

        iterations = pow(2, maxDepth - depth + minDepth);

        check = 0;

        for (i = 1; i <= iterations; i++)
        {
            tempTree = BottomUpTree(i, depth);
            check += ItemCheck(tempTree);
            DeleteTree(tempTree);

            tempTree = BottomUpTree(-i, depth);
            check += ItemCheck(tempTree);
            DeleteTree(tempTree);
        } /* for(i = 1...) */

        // printf
        (
            "%li\t trees of depth %u\t check: %li\n",
            iterations * 2,
            depth,
            check
        );
    } /* for(depth = minDepth...) */
    return 0;
}

FAST_SWITCH_DECORATOR
i64 long_lived_tree_create_handle_body(i64 *env) {
    treeNode_t **longLivedTree = (treeNode_t **)env[0];
    int maxDepth = (int)env[1];
    *longLivedTree = BottomUpTree(0, maxDepth);
    return 0;
}

FAST_SWITCH_DECORATOR
i64 long_lived_tree_test_handle_body(i64 *env) {
    treeNode_t *longLivedTree = (treeNode_t *)env[0];
    unsigned int maxDepth = (unsigned int)env[1];

    // printf
    (
        "long lived tree of depth %u\t check: %li\n",
        maxDepth,
        ItemCheck(longLivedTree)
    );
    return 0;
}

/******
 build & benchmark results

BUILD COMMANDS FOR: binarytrees.gcc

Thu Sep 14 00:25:13 PDT 2006

/usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -funroll-loops -march=pentium4 -lm binarytrees.c -o binarytrees.gcc_run

=================================================================
COMMAND LINE (%A is single numeric argument):

binarytrees.gcc_run %A
N=16

PROGRAM OUTPUT
==============
stretch tree of depth 17	 check: -1
131072	 trees of depth 4	 check: -131072
32768	 trees of depth 6	 check: -32768
8192	 trees of depth 8	 check: -8192
2048	 trees of depth 10	 check: -2048
512	 trees of depth 12	 check: -512
128	 trees of depth 14	 check: -128
32	 trees of depth 16	 check: -32
long lived tree of depth 16	 check: -1
skinny tree of depth 17          check: 17
*********/