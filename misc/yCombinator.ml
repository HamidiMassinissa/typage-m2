let y_combinator f =
  f (fun x -> f x x) (fun x -> f x x)

let fact f = function
    0 -> 1
  | n -> n * (f (n-1))

let fact_5 = y_combinator (fact 5)
