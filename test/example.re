let rec fact = (n) => if (n == 1) { 1 } else { n * fact (n - 1)};

let%test _ = fact(5) == 120;
