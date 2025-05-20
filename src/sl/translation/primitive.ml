open SLsyntax

let prim_sigs = [
  ("listRange", ([TInt; TInt], TNode TInt));
  ("listPrint", ([TNode TInt], TNode TInt));
  ("listMax", ([TNode TInt], TInt));
  ("readInt", ([], TInt));
  ("printInt", ([TInt], TInt));
  ("printFloat", ([TFloat], TInt));
  ("printChar", ([TChar], TInt));
  ("stringMake", ([TChar; TInt], TStr));
  ("stringSubStr", ([TStr; TInt; TInt], TStr));
  ("stringCharAt", ([TStr; TInt], TChar));
  ("stringLen", ([TStr], TInt));
  ("boxFloat", ([TFloat], TInt));
  ("unboxFloat", ([TInt], TFloat));
  ("floatAdd", ([TInt; TInt], TInt));
  ("floatSub", ([TInt; TInt], TInt));
  ("floatMul", ([TInt; TInt], TInt));
  ("floatDiv", ([TInt; TInt], TInt));
  ("floatPow", ([TInt; TInt], TInt));
  ("floatExp", ([TInt], TInt));
  ("floatNeg", ([TInt], TInt));
  ("floatRand", ([], TInt));
  ("floatPi", ([], TInt));
  ("floatCos", ([TInt], TInt));
  ("floatSin", ([TInt], TInt));
  ("floatSqrt", ([TInt], TInt));
  ("floatLog", ([TInt], TInt));
  ("floatLt", ([TInt; TInt], TInt));
  ("floatLeq", ([TInt; TInt], TInt));
  ("mathAbs", ([TInt], TInt));
  ("arrayPrint", ([TArray TInt], TInt));
  ("arrayPrintChars", ([TArray TChar], TInt));
  ("pairMake", ([TInt; TInt], TInt));
  ("pairFst", ([TInt], TInt));
  ("pairSnd", ([TInt], TInt));
  ("strPrint", ([TStr], TInt));
  ("strConcat", ([TStr; TStr], TStr));
  ("strEq", ([TStr; TStr], TBool));
  ("strcmp", ([TStr; TStr], TInt));
  ("strlen", ([TStr], TInt));
  ("strCharAt", ([TStr; TInt], TInt))
]

let prim_polymophic_sigs = [
  ("check", ("'a", [TBool], TVar "'a"));
  ("error", ("'a", [TStr], TVar "'a"));

  ("listNode", ("'a", [TVar "'a"; TNode (TVar "'a")], TNode (TVar "'a")));
  ("listEnd", ("'a", [], TNode (TVar "'a")));
  ("listIsEmpty", ("'a", [TNode (TVar "'a")], TBool));
  ("listHead", ("'a", [TNode (TVar "'a")], TVar "'a"));
  ("listTail", ("'a", [TNode (TVar "'a")], TNode (TVar "'a")));
  ("listSetHead", ("'a", [TNode (TVar "'a"); TVar "'a"], TVar "'a"));
  ("listSetTail", ("'a", [TNode (TVar "'a"); TNode (TVar "'a")], TInt));
  ("listAppend", ("'a", [TNode (TVar "'a"); TNode (TVar "'a")], TNode (TVar "'a")));
  ("listLen", ("'a", [TNode (TVar "'a")], TInt));
  ("listAt", ("'a", [TNode (TVar "'a"); TVar "'a"], TVar "'a"));

  ("treeNode", ("'a", [TVar "'a"; TTree (TVar "'a"); TTree (TVar "'a")], TTree (TVar "'a")));
  ("treeLeaf", ("'a", [], TTree (TVar "'a")));
  ("treeIsEmpty", ("'a", [TTree (TVar "'a")], TBool));
  ("treeLeft", ("'a", [TTree (TVar "'a")], TTree (TVar "'a")));
  ("treeRight", ("'a", [TTree (TVar "'a")], TTree (TVar "'a")));
  ("treeValue", ("'a", [TTree (TVar "'a")], TVar "'a"));

  ("queueMake", ("'a", [], TQueue (TVar "'a")));
  ("queueIsEmpty", ("'a", [TQueue (TVar "'a")], TBool));
  ("queueEnq", ("'a", [TQueue (TVar "'a"); TVar "'a"], TVar "'a"));
  ("queueDeq", ("'a", [TQueue (TVar "'a")], TVar "'a"));
  ("queueLen", ("'a", [TQueue (TVar "'a")], TInt));

  ("arrayMake", ("'a", [TInt], TArray (TVar "'a")));
  ("arrayMakeInit", ("'a", [TInt; TVar "'a"], TArray (TVar "'a")));
  ("arrayLen", ("'a", [TArray (TVar "'a")], TInt));
  ("arrayAt", ("'a", [TArray (TVar "'a"); TInt], TVar "'a"));
  ("arraySet", ("'a", [TArray (TVar "'a"); TInt; TVar "'a"], TVar "'a"));
  ("arrayPush", ("'a", [TArray (TVar "'a"); TVar "'a"], TVar "'a"));
  ("arrayPop", ("'a", [TArray (TVar "'a")], TVar "'a"));
]

let prim_variable_args_sigs = [
  ("printf", TInt)
]