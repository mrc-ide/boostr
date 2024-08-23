#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]]
doubles ab_cpp(integers t, doubles td, doubles cs, doubles rho, doubles ds, doubles dl) {
  int n = t.size();

  int n_set = ds.size();
  writable::doubles rs(n_set);
  writable::doubles rl(n_set);
  for(int i = 0; i < n_set; ++i) {
    rs[i] = log(2) / ds[i];
    rl[i] = log(2) / dl[i];
  }

  writable::doubles ab(n);
  int t_cur = 0;
  double cs_cur = cs[0];

  int t_index = 0;
  for(int i = 0; i < n; ++i) {  // Loop should start at 0 for proper indexing
    // Check if we need to increment t_index
    if (t_index + 1 < td.size() && t[i] == td[t_index + 1]) {
      t_index++;
    }

    // Check for down shifts on boostt
    if(t[i] > 1){
      if (t[i] == td[t_index]) {
        cs_cur = cs[t_index];
        if (ab[i - 1] > cs_cur) {
          cs_cur = ab[i - 1];
        }
      }
    }

    // Set t_cur relative to the current time division
    t_cur = t[i] - 1;
    if (t_index > 0) {
      t_cur = t[i] - td[t_index];
    }

    // Compute ab[i]
    ab[i] = cs_cur * (rho[t_index] * exp(-rs[t_index] * t_cur) + (1 - rho[t_index]) * exp(-rl[t_index] * t_cur));
  }

  return ab;
}
