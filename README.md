# SchEmacs

Functional Reconstruction

The implementation of Schemacs uses a set of virtual registers (global variables) in order to avoid passing too many parameters between functions, and there is a self-incrementing relationship between global variables.

However, the use of too many global variables leads to a series of problems. The calling structure between functions becomes unclear and difficult to test. The function relies too much on the calling order.

The goal of this branch is to avoid the use of virtual registers (global variables) and to reconstruct the code in a more functional way, in order to solve the above problems.





