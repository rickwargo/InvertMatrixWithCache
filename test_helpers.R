
isIdentity <- function(m) {
  # Determine if the specified matrix is an identity matrix.
  #
  # Args:
  #   m: matrix to check for identity
  #
  # Returns:
  #   boolean stating if the specified matrix is an identity matrix
  
  all(m[!diag(nrow(m))] == 0) && all(m[diag(nrow(m))] == 1)
}

hilbertMatrix <- function(n) {
  # Generate a Hilbert matrix of length 'n' x 'n'.
  #
  # Args:
  #   n: matrix row/col length
  #
  # Returns:
  #   Hilbert matrix of length 'n' x 'n' 
  
  i <- 1:n
  1 / outer(i - 1, i, "+")
}