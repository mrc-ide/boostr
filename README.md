
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boostr

<!-- badges: start -->

[![R-CMD-check](https://github.com/mrc-ide/boostr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mrc-ide/boostr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/mrc-ide/boostr/graph/badge.svg)](https://app.codecov.io/gh/mrc-ide/boostr)
<!-- badges: end -->

This is a generic implementation of a combined vaccine and vaccine
efficacy modelling frame work first proposed in [White et al
(2015)](https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00239-X/fulltext).

The model consists of two parts. The first predicts antibody titres over
time following initital dose(s) and subsequent booster doses.

$$
titre(t) = titre_{dose} \left( \rho_{dose} e^{-r_s (t - t_{dose})} + (1 - \rho_{dose}) e^{-r_l (t - t_{dose})} \right)
$$

Where subscript $dose$ indexes for the primary series dose(s) or
subsequent booster doses, $titre$ is the maximum titre on receipt of a
dose or booster, $rho$ the proportion of the response that is
short-lived and $r_s = \frac{\log_e(2)}{d_s}$ and
$r_l = \frac{\log_e(2)}{d_l}$ where $d_s$ and $d_l$ are the half-lives
of the short and long lived components of the antibody response
respectively.

The second translates antibody titre to vaccine efficacy using a
parameterised dose response curve.

$$
V(t) = V_{max} \left( 1 - \frac{1}{1 + \left( \frac{CS(t)}{\beta} \right)^\alpha } \right)
$$ Where $V_{max}$ is the maximum vaccine efficacy, $alpha$ the shape
parameter and $beta$ the scale parameter
