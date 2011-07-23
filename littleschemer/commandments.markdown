The First Commandment
---

When recurring on a list of atoms, _lat_, ask two questions about it: _(null? lat)_ and else.

When recurring on a number, _n_, ask two questions about it: _(zero? n)_ and else.

When recurring on a list of S-expressions, _l_, ask three questions about it: _(null? l)_, _(atom? (car l))_, and else.

The Second Commandment
---

Use _cons_ to build lists.

The Third Commandment
---

When building a list, describe the typical element, and then _cons_ it onto the natural recursion.

The Fourth Commandment
---

Always change at least one argument while recurring. When recurring on a list of atoms, _lat_, use _(cdr lat)_. When recurring on a number, _n_, use _(sub1 n)_. And when recurring on a list of S-expressions, _l_, use _(car lat)_ and _(cdr lat)_ if neither _(null? l)_ nor _(atom? (car l))_ are true.

It must be changed to be closer to termination. The changing argument must be tested in the termination condition: 

when using _cdr_, test termination with _null?_ and 
when using _sub1_, test termination with zero?

The Fifth Commandment
---

When building a value with _+_, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition.

When building a value with _*_, always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication.

When building a value with _cons_, always consider () for the value of the terminating line.

The Sixth Commandment
---

Simplify only after the function is correct.

The Seventh Commandment
---

Recur on the _subparts_ that are of the same nature:

+ On the sublists of a list.
+ On the subexpressions of an arithmetic expression.

The Eigth Commandment
---

Use help functions to abstract from representations.

The Ninth Commandment
---

Abstract common patterns with a new function.

The Tenth Commandment
---

Build functions to collect more than one value at a time.

