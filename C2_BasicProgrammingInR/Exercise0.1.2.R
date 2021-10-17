sixes <- function(n) {
  points <- sample(1:6, n, replace = TRUE)
  six <- points[points == 6]
  if (length(six) == 0) {
    return ("fALSE");
  }
  return ("TRUE");
}

ans <- sixes(5)
ans

play <- function(n, N) {
  ok <- 0;
  for (i in 1:N) {
    if (sixes(n) == "TRUE") {
      ok = ok + 1;
    }
  }
  return (ok / N)
}

p <- play(5, 20)
p