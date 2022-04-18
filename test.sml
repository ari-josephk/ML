datatype ('a,'b) tree = Leaf of 'a
| Node of 'a * 'b;

fun test3 (Leaf a) preda predb = Node(a, preda a)
| test3 (Node (a, b)) preda predb = Node (a, predb (preda a) b);

1234 div 10
~3