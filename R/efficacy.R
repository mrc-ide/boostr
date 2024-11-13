#' Estimate vaccine efficacy
#'
#' @param titre Vector of antibody titres
#' @param max_efficacy Maximum vaccine efficacy
#' @param alpha Shape parameter
#' @param beta Scale parameter
#'
#' @return Vector of antibody efficacies for each titre
#' @export
efficacy <- function(titre, max_efficacy, alpha, beta){
  if(any(titre < 0)){
    stop("All titres must be > 0")
  }
  if(any(max_efficacy < 0) | any(max_efficacy > 1)){
    stop("max_efficacy must be between 0 and 1")
  }
  if(alpha < 0){
    stop("alpha must be > 0")
  }
  if(beta < 0){
    stop("beta must be > 0")
  }

  max_efficacy * (1 - (1 / (1 + ((titre / beta) ^ alpha))))
}
