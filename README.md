To compile the project one should install cabal, clone this repository and type 
```
cabal build
```
To run the compiler, one should type `cabal run`, then enter the program code and press `Ctrl+D`.
It's also possible to pipe file into `cabal run` as so `cat program | cabal run`. 

In order to run test one can execute
```
cabal test
```

This will run some tests for Lexer and Parser, but also will compile programs.
Here are some examples of programs, that are tested in `test/MainSpec.hs`:
```
# Programs consist of a few let statements followed by expression that gets evaluated
let K = fun x y -> x;
let S = fun x y z -> (x z) (y z);
S K K 7;
# This program prints 7
```
```
# There are builtin bools, numbers, lists and some functions such as plus, minus, and, or, cond,
# etc.
let even = fun x -> cond (eq x 0) true (odd (minus x 1));
let odd = fun x -> cond (eq x 0) false (even (minus x 1));
and (even 74) (odd 75);
```
```
# There is let name = val in expr expression
let fib = fun x -> 
              if less x 2 then
                  x
              else
                  plus (fib (minus x 1)) (fib (minus x 2));
in fib 22;
```
```
let isdiv = fun n m -> eq (mod n m) 0; 
let isprimecounter = fun i n -> 
  if lesseq (mul i i) n then
      if isdiv n i then
          false
      else
          isprimecounter (plus i 1) n
  else
      true;
let isprime = fun n -> isprimecounter 2 n;
isprime 7919;
```
```
let append l k ->
    if l == nil then
        k
    else
        cons (hd l) (append (tl l) k);
let merge l k ->
    if l == nil || k == nil then
        append l k
    else
        if hd l < hd k then
            cons (hd l) (merge (tl l) k)
        else
            cons (hd k) (merge l (tl k));
let length l ->
    if l == nil then
        0
    else
        length (tl l) + 1;
let takeFirst n l ->
    if n == 0 then
        nil
    else
        cons (hd l) (takeFirst (n - 1) (tl l));
let dropFirst n l ->
    if n == 0 then
        l
    else
        dropFirst (n - 1) (tl l);
let sort l ->
    let len = length l in
    let halfLen = len / 2 in
    let fHalf = takeFirst halfLen l in
    let sHalf = dropFirst halfLen l in
    if len < 2 then l else
    merge (sort fHalf) (sort sHalf);

sort [1, 43, 8, 2, 3, 1, 73, 9, 12];
```
