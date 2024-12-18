#' Renyi
#'
#' This function calculates renyi entropy
#' @export
renyi_entropy <- function(probabilities, alpha, base = 2) {
  # Check if probabilities sum to 1
  if (abs(sum(probabilities) - 1) > 1e-6) {
    stop("Probabilities must sum to 1.")
  }
  
  # Handle special cases for alpha
  if (alpha <= 0) {
    stop("Alpha must be greater than 0.")
  } else if (alpha == 1) {
    # Shannon entropy as a special case
    return(-sum(probabilities * log(probabilities, base = base)))
  }
  
  # Calculate Rényi entropy for alpha != 1
  sum_p_alpha <- sum(probabilities^alpha)
  renyi <- (1 / (1 - alpha)) * log(sum_p_alpha, base = base)
  return(renyi)
}
