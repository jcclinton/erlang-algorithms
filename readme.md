### algoritms and data structures in erlang

Currently includes:

* trie
* dijkstra
* kd\_tree
* quadtree
* genetic

###### Trie

Stores words in a space efficient manner. Does not store values. Can return all possibilities using `trie:lookup(Word, Trie)`. For example if `random`, `tea`, `tray`, and `truce` are in the trie, calling `trie:lookup("tr", Trie)` will return `[<<"tray">>, <<"truce">>]`.

###### Dijkstra

implementation of Dijkstra's graph search algorithm. `dijkstra:test()` is an example implementation.


###### kd\_tree

`kd_tree:create(PointList)` will create a tree. `PointList` is a list of point tuples of whatever dimension you want. You can also use `kd_tree:insert(Point, Tree)` to dynamically insert points and `kd_tree:remove(Point, Tree)`, but the tree will become unbalanced with the current implementation. Tree can be manually rebalanced with `kd_tree:rebalance(Tree)`. `kd_tree:nearest_neighbor(Point, Tree)` will return the nearest neighbor to that point.


###### quadtree

Implementation of a quadtree. API is: `new(Range)`, `insert(Point)`, `query_range(Range, Quad)` where `Range` is a point with a halfwidth associated with it (ie a bounding box).


###### genetic algorithm

This is a specific implementation using [this](http://www.ai-junkie.com/ga/intro/gat3.html) example. Currently it often gets caught up in suboptimal solutions. But that may be because the mutation rate is set very low, 1 in 10000 children have a bit in their chromosome mutated.
