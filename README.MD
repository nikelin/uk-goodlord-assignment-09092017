### Goodlord, Technical Assessment

##### Console

Both tasks are accessible via a common CLI. It was quickly drafted using 
`scalaz-effect` and `IO` monad.
 
There is also a limited validation support for a user provided input.

##### Task 1: Longest common sub-sequence 

Implementation is based on DP approach and uses memoization capabilities 
provided by Scalaz (`Memo.immutableHashMap`).

Returns empty string if there is no commonality between two given strings.

Estimated complexity for both space and time is `O(m * n)`, due to recursive caching 
strategy.

There is basic unit tests that are covering three main use-cases: `LcsSpec`.

###### Directions to run:

```sbtshell
$> sbt run
Task (1 - LCS, 2 - Expressions): 
1
Left string:
AABACDA
Right string:
DACBBCAD
Result = ABCA 
```

##### Task 2: Expressions derivation

Task solution is based on a total permutations approach (sub-sets x operations).
 
Problem is separated onto three categories:
- Finding all combinations that a given integers set and allowed operations form together
- Reducing particular expression to a string (Show) 
- Reducing particular expression to a value (Run)

Expressions forms `ADT` can easily be changed to a `Free`-like structure. For now I decided to keep it as
simple as possible. But a general pattern is clear here.

Mainly because of a permutative nature of an algorithm used, the solution overall complexity
is close to `(n ^ 2) * n!` when used to resolve all possible equations.

Taking into account this problem, solution has to use lazy calculations based on scala `Stream`-s.
`Stream` helps to avoid eager evaluation of all possible combinations. Instead they computation happens
based on a demand (`take`, `drop`, etc). But it is also just partially solves the problem. If enough values
provided and target value that can never be achieved, program can possibly just run forever (or for an unacceptably 
long time).

As a possible improvement to the execution time it is possible to employ genetic programming  
approach. But it is also limited to finding an exact match for a given target value. 

Genetic approach is briefly described here (JS): http://starcolon.com/blog/algorithm-solve-game-24/

Also, as a minor trade-off to gain some performance, instead of expressions evaluations via `evaluateExpression`, they
can be evaluated at they creation with value being stored in a field of a `case class` itself. In this particular case such solution probably looks
quite good, as respective `ADT` are immutable and they evaluation logic is unlikely to be changed.

###### Directions to run

```sbtshell
$> sbt run
Task (1 - LCS, 2 - Expressions): 
Enter a positive integer or any other key to finish: 
Enter a positive integer or any other key to finish: 2
Enter a positive integer or any other key to finish: 3,2
Enter a positive integer or any other key to finish: 5,3,2
Enter a positive integer or any other key to finish: 6,5,3,2
Enter a positive integer for a target number: 
42
(6 * (5 + 2))
```

##### Tests

Unit tests are integrated into the SBT build flow:

`sbt test`


 

  


 
