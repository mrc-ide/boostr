#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]]
doubles efficacy_cpp(doubles titre, double max_efficacy, double alpha, double beta) {
  int n = titre.size();
  writable::doubles ef(n);

  for(int i = 0; i < n; ++i) {
    ef[i] = max_efficacy * (1 - (1 / (1 + std::pow(titre[i] / beta, alpha))));
  }

  return ef;

}
