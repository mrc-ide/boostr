#' Estimate antibody titres
#'
#' @param timesteps Number of timesteps to output titres for
#' @param dose_timesteps Timesteps of doses
#' @param init_titres Titres at initital does or subsequent boost times
#' @param prop_short Proportion of the response that is short lived for each dose period
#' @param dur_short Duration of the short-lived response for each dose period
#' @param dur_long Duration of the long-lived response for each dose period
#' @param cpp Use cpp titre model
#'
#' @return Vector of antibody tires for 1:timesteps
#' @export
ab <- function(timesteps, dose_timesteps, init_titres, prop_short, dur_short, dur_long, cpp = TRUE){
  if(timesteps < 1){
    stop("timesteps must be positive")
  }
  if(!(all(dose_timesteps > 0) && all(diff(dose_timesteps) >= 0))){
    stop("dose_timesteps must be positive and montonically increasing")
  }
  if(any(init_titres <= 0)){
    stop("All init_titres must be > 0")
  }
  if(any(prop_short < 0) | any(prop_short > 1)){
    stop("prop_short must be between 0 and 1")
  }
  if(any(dur_short > dur_long)){
    stop("dur_short should be < dur_long")
  }

  timesteps <- 1:timesteps

  if(cpp){
    titres <- ab_cpp(timesteps, dose_timesteps, init_titres, prop_short, dur_short, dur_long)
  } else {
    titres <- ab_r(timesteps, dose_timesteps, init_titres, prop_short, dur_short, dur_long)
  }

  return(titres)
}

ab_r <- function(t, dose_timesteps, init_titres, prop_short, dur_short, dur_long){
  t_index <- findInterval(t, dose_timesteps)
  t_rel <- t - dose_timesteps[t_index]
  rs <- log(2) / dur_short
  rl <- log(2) / dur_long

  ab <- rep(NA, length(t))
  for(i in t){
    t_index_i <- t_index[i]
    t_rel_i <- t_rel[i]
    # Check boost doesn't cause downwards drop in ab titre:
    if(i > 1){
      if(i == dose_timesteps[t_index_i]){
        if(ab[i - 1] > init_titres[t_index_i]){
          init_titres[t_index_i] <- ab[i - 1]
        }
      }
    }
    ab[i] <- init_titres[t_index_i] * ((prop_short[t_index_i] * exp(-rs[t_index_i] * t_rel_i)) + ((1 - prop_short[t_index_i]) *  exp(-rl[t_index_i] * t_rel_i)))
  }

  return(ab)
}
