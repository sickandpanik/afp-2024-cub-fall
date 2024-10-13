ghost predicate is_node(graph: seq<seq<int>>, n: int)
{
    0 <= n < |graph|
}

ghost predicate is_graph(graph: seq<seq<int>>)
{
    forall i :: is_node(graph, i) ==>
        forall k :: 0 <= k < |graph[i]| ==> is_node(graph, graph[i][k])
}

ghost predicate is_graph_path(graph: seq<seq<int>>, path: seq<int>)
{
    (forall i :: 0 <= i < |path| ==> is_node(graph, path[i])) &&
    (forall i :: 0 <= i < |path| - 1 ==> path[i+1] in graph[path[i]])
}

ghost predicate path_ends_are(path: seq<int>, start: int, end: int)
{
    |path| > 0 && path[0] == start && path[|path|-1] == end
}

ghost predicate path_crosses(path: seq<int>, visited: set<int>) 
{
    exists i :: 0 <= i < |path| - 1 && path[i] in visited && path[i+1] !in visited
}

// graph - adjececny list representation of a graph
// want to check if we can reach `end` vertex `start` vertex
// we are using standard bfs algorithm

method bfs(graph : seq<seq<int>>, start : int, end : int) returns (b : bool)
    requires is_node(graph, start)
    requires is_node(graph, end)
    requires is_graph(graph) // valid graph
    ensures b ==> exists p : seq<int> :: is_graph_path(graph, p) && path_ends_are(p, start, end) 
    ensures (exists p : seq<int> :: is_graph_path(graph, p) && path_ends_are(p, start, end)) ==> b 
    decreases * // this `decreases` statement states that this method can possibly do not terminate
    // you can optionally remove this `decreases`, uncomment `decreases |q| + unvisited` in while statements
    // (where unvisited denotes number of unvisited vertices) and prove termination
{
    if start == end {
        b := true;
        return;
    }
    b := false;
    var q := [start]; // queue
    var visited : set<int> := {start}; // set of visited nodes

    while |q| > 0
        // uncomment this invariant and prove it 
        // it will help to prove second postcondition
        // invariant forall p : seq<int> :: is_graph_path(graph, p) && path_ends_are(p, start, end) ==> path_crosses(p, visited) 
        // path_crosses means that on every path to `end` there is an unvisited vertex
        decreases *
        // decreases |q| + unvisited
    {
        var node := q[0];
        q := q[1..];
        var neighbors := graph[node];
        var i := 0;
        while i < |neighbors|
        {
            var neighbor := neighbors[i];
            if neighbor !in visited {
                visited := visited + {neighbor};
                if neighbor == end {
                    b := true;
                    return;
                }
                q := q + [neighbor];
            }
            i := i + 1;
        }
    }
    assert forall n : int :: is_node(graph, n) && n in visited ==> (forall i : int :: 0 <= i < |graph[n]| ==> graph[n][i] in visited);
    // this assertion states that there is no visited and unvisited vertices connected by an edge
    // it helps to prove second postcondition (think how?)
    // write an invariant that will help to prove it
}