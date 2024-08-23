#' Estimate vaccine efficacy
#'
#' @param titre Vector of antibody titres
#' @param max_efficacy Maximum vaccine efficacy
#' @param alpha Shape parameter
#' @param beta Scale parameter
#' @param cpp Use cpp efficacy model
#'
#' @return Vector of antibody efficacies for each titre
#' @export
efficacy <- function(titre, max_efficacy, alpha, beta, cpp = TRUE){
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

  if(cpp){
    ef <- efficacy_cpp(titre, max_efficacy, alpha, beta)
  } else {
    ef <- efficacy_r(titre, max_efficacy, alpha, beta)
  }
}

efficacy_r <- function(titre, max_efficacy, alpha, beta){
  max_efficacy * (1 - (1 / (1 + ((titre / beta) ^ alpha))))
}
