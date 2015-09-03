# Platon
## Typesystem

Platon supports a wide variety of different algebraic structures and efficient implementations of them, similar to Magma.

The system is supposed to be general enough so that for each notion category the user implements, together with potential monoidal product.

### Examples

A computer system has some finite sized main memory, caches, disks, diskcaches etc., denote them by M_{i}. Then whenever it is possible to transfer data from M_{i} to M_{j} denote this by an arrow A_{ij}.

Assume for the moment that each of the memory spaces are linear addressable, that is they are all characterized by a tuple (V,d), where V is the base configuration space of the memory and d is the dimension.

For a map $A_{ij}$ to exist at all one then has to have a map from $V_{i}^{k}$ to $V_{j}^{l}$ for some $k < d_{i}, l < d_{j}$.

q : Queue[T]
enqueue : i × q → q
dequeue : q → (ε | i) × q

### Indextypes

