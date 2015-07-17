## Observable, parametric views: a pragmatic ... ? ##

### Requirements ###

+ We have shared views!
+ We need notification

### Notes ###

+ Lens a a b b -> Lens 
+ Lens a a c d -> Access restriction
+ We build a dependency tree/graph
+ Inductively defined

### Questions ###

1. Why different read and write types?
2. Different read and write parameters?
3. Why parameters?
4. ID generation? (identifying nodes in the graph)
   Type driven?
5. Changing the parameter type?

### Answers ###

1. Only when E.f: w = f(r) and only as a last lens
2. No
3. Observers, Efficiency (less read)
4. 
5. Not in a general way, because that causes the
"multiple read" problem. Only *parameter translation* and *split*

### Interface ###

type View r w
type PView (pr r) (pw w)

### Optimizations ###

* Reducing graph size
* "Caching" of read value during write. See "multiple read" problem


