#' Shannon
#'
#' This function calculates shannon entropy
#' @export
shannon_entropy = function(probabilities, base = 2) {
  total_SE = 0
  for (element in probabilities){
    total_SE = total_SE + element*log(element, base)
  }
  return(-total_SE)
}
