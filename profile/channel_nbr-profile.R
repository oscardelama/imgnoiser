build.test.data <- function() {
  t(combn(1000,2))
}

channel.nbr.naive <- function(row, col) {
  return((row %% 2) + 2*(col %% 2))
}

channel.nbr.bitw <- function(row, col) {
  bitwAnd(row, 1) + bitwShiftL(bitwAnd(col, 1),1)
}

cppFunction('int channel_nbr_modC(int row, int col) {
  int result = (row % 2) + (col % 2) << 1;
  return result;
}')

cppFunction('int channel_nbr_andC(int row, int col) {
  int result = (row & 1) + (col & 1) << 1;
  return result;
}')

prof.test <- function(test.data) {
  for (x in 1:1) {
    for (r in 1:nrow(test.data)) {
      row <- test.data[r,1]
      col <- test.data[r,2]
      r <- channel.nbr.naive(row, col)
      r <- channel.nbr.bitw(row, col)
      r <- channel_nbr_modC(row, col)
      r <- channel_nbr_andC(row, col)
    }
  }
}
