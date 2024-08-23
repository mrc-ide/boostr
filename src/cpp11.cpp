// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// antibody.cpp
doubles ab_cpp(integers t, doubles td, doubles cs, doubles rho, doubles ds, doubles dl);
extern "C" SEXP _boostr_ab_cpp(SEXP t, SEXP td, SEXP cs, SEXP rho, SEXP ds, SEXP dl) {
  BEGIN_CPP11
    return cpp11::as_sexp(ab_cpp(cpp11::as_cpp<cpp11::decay_t<integers>>(t), cpp11::as_cpp<cpp11::decay_t<doubles>>(td), cpp11::as_cpp<cpp11::decay_t<doubles>>(cs), cpp11::as_cpp<cpp11::decay_t<doubles>>(rho), cpp11::as_cpp<cpp11::decay_t<doubles>>(ds), cpp11::as_cpp<cpp11::decay_t<doubles>>(dl)));
  END_CPP11
}
// efficacy.cpp
doubles efficacy_cpp(doubles titre, double max_efficacy, double alpha, double beta);
extern "C" SEXP _boostr_efficacy_cpp(SEXP titre, SEXP max_efficacy, SEXP alpha, SEXP beta) {
  BEGIN_CPP11
    return cpp11::as_sexp(efficacy_cpp(cpp11::as_cpp<cpp11::decay_t<doubles>>(titre), cpp11::as_cpp<cpp11::decay_t<double>>(max_efficacy), cpp11::as_cpp<cpp11::decay_t<double>>(alpha), cpp11::as_cpp<cpp11::decay_t<double>>(beta)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_boostr_ab_cpp",       (DL_FUNC) &_boostr_ab_cpp,       6},
    {"_boostr_efficacy_cpp", (DL_FUNC) &_boostr_efficacy_cpp, 4},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_boostr(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
