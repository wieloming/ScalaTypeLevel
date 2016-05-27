sealed trait Natural {
  type * [_ <: Natural] <: Natural
  type + [_ <: Natural] <: Natural
}

object Zero extends Natural {
  type * [X <: Natural] = Zero.type
  type + [X <: Natural] = X
}

class Successor[N <: Natural] extends Natural {
  type + [X <: Natural] = Successor[N + X]
  type * [X <: Natural] = Successor[N * X + X]
}