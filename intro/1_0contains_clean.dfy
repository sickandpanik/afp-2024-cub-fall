
// `mcontained` - checks if 1st sequence is contained in 2nd sequence
// sequences are strictly sorted

// seq<int> - data type for sequence of integers
// a : seq<int> 
// |a| - denotes length of sequence
// a[i] - refer to the i-th element of the sequence
// x in a, x !in a
// a[i..j] - [a[i], a[i+1], ..., a[j - 1]]


// 
// [(1), (2), (7)]
// 
// [1, 2, 5, 7, 8]


// [1, 2, (3), 6] // v[i] < w[j]
// [1, 2, (7), 8]


// Can use predicates in invariants
predicate StrictlySorted(v : seq<int>) {
	forall i, j :: 0 <= i < j < |v| ==> v[i] < v[j]
}


method mcontained(v : seq<int>, w : seq<int>) returns (b : bool) 
	// requires |v| <= |w|
	requires StrictlySorted(v) && StrictlySorted(w)
	ensures b ==> forall k :: 0 <= k < |v| ==> v[k] in w 
	ensures (forall k :: 0 <= k < |v| ==> v[k] in w) ==> b 
{
	var i := 0;
	var j := 0;
	while (i < |v| && j < |w| && (v[i] >= w[j]))
		invariant 0 <= i <= |v| 
		invariant 0 <= j <= |w|
		invariant forall k :: 0 <= k < i ==> v[k] in w 
		invariant i < |v| ==> !(v[i] in w[..j]) // v[i], w[..j]
	{	
		if (v[i] == w[j]) {
			i := i + 1;
		}
		j := j + 1;
		
	}
	b := i == |v|;
}

method Main() {
    print("Hello, world!" + "\n");
    var res := mcontained([1, 2, 3], [1, 2, 3, 4, 5]);
    print res, "\n";
}
