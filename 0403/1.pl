tree --> "*".
tree --> "(", tree, tree, ")".

tree2 --> "(", tree2, tree2, ")".
tree2 --> "*".

tree3(node(Left, Right)) --> "(", tree3(Left), tree3(Right), ")".
tree3(leaf) --> "*".
