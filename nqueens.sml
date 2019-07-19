
(*small helper functions, we used member and set in class, rangeCheck makes sure
* no numbers exceed the size of the board, abs is for absolute value, checkSize
* gets total size of board thus far*)
fun member(x, []) = false
  | member(x, h::t) = 
        if x = h orelse h<1 then
            true
        else
            member(x, t)

fun set([]) = true
  | set(H::T) =
        if member(H, T) then
            false
        else
            set(T)

fun rangeCheck(x, []) = false
  | rangeCheck(x, h::t) =
    if(h > x) then
       true
     else
       rangeCheck(x, t)

fun abs(x) =
  if x<0 then
    ~x
   else
     x

fun checkSize([], x) = x-1
  | checkSize(h::t, x) =
    checkSize(t, x+1)

(*locates diagonals by comparing x-x and y-y of two points, if the same, they
* are diagonals*)
fun diagonal(index, value, counter, []) = false
  | diagonal(index, value, counter, h::t) = 
     if abs(value-h) = abs(index-counter) then
      true
     else
       diagonal(index, value, counter+1, t)

(*master safe check function, makes sure no duplicate numbers, no diagonals, and
* no numbers above the range of the board*)
fun safe([], index, size) = true
  | safe(h::t, index, size) = 
    if set(h::t) = false orelse diagonal(index, h, index+1, t) orelse
    rangeCheck(size, h::t) then
      false
    else
      safe(t, index+1, size) 

(*if the board is safe, and the size of the board is equal to original x, done*)
fun checkComplete(L, x) =
  if safe(L, 1, x) andalso checkSize(L, 1) = x then
     true
  else
     false

(*increment just increases the current head by 1 until a safe option is found and sends a new list back to the makeBoard function to use*)
fun increment([], n) = []
  | increment(h::t, n) =
  (* check if safe or going off board*)
    if(safe(h+1::t, 1, n)) andalso h+1 < n+1 then
      (*returns a list of items to continue making a board with*)
       h+1::t
    else if h+1 < n+1 then
      increment(h+1::t, n)
    else
      increment(t, n)

(*function that adds queens onto the list recursively*)
fun makeBoard([], numToAdd, size) = makeBoard([1], 1, size)
  | makeBoard(L, numToAdd, size) =
    (*check if done*)
    if checkComplete(L, size) then
      L
    (*checks if it is safe to add a new prospective number to the collection*)
    else if safe(numToAdd::L, 1, size) = true then
      makeBoard(numToAdd::L, 1, size)

    (*backtracking to a previous queen here*)
    else if numToAdd > size then
      if increment(L, size) = [] then
        []
      else
        makeBoard(increment(L, size), 1, size)

    else
      makeBoard(L, numToAdd+1, size)

fun nqueens(x) =
      makeBoard([], 1, x)
      


