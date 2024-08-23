#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]]
doubles ab_cpp(integers t, doubles dose_timesteps, doubles init_titres, doubles prop_short, doubles dur_short, doubles dur_long) {
  int n = t.size();

  int n_set = dur_short.size();
  writable::doubles rs(n_set);
  writable::doubles rl(n_set);
  for(int i = 0; i < n_set; ++i) {
    rs[i] = log(2) / dur_short[i];
    rl[i] = log(2) / dur_long[i];
  }

  writable::doubles ab(n);
  int t_cur = 0;
  double init_titres_cur = init_titres[0];

  int t_index = 0;
  for(int i = 0; i < n; ++i) {  // Loop should start at 0 for proper indexing
    // Check if we need to increment t_index
    if (t_index + 1 < dose_timesteps.size() && t[i] == dose_timesteps[t_index + 1]) {
      t_index++;
    }

    // Check for down shifts on boostt
    if(t[i] > 1){
      if (t[i] == dose_timesteps[t_index]) {
        init_titres_cur = init_titres[t_index];
        if (ab[i - 1] > init_titres_cur) {
          init_titres_cur = ab[i - 1];
        }
      }
    }

    // Set t_cur relative to the current time division
    t_cur = t[i] - 1;
    if (t_index > 0) {
      t_cur = t[i] - dose_timesteps[t_index];
    }

    // Compute ab[i]
    ab[i] = init_titres_cur * (prop_short[t_index] * exp(-rs[t_index] * t_cur) + (1 - prop_short[t_index]) * exp(-rl[t_index] * t_cur));
  }

  return ab;
}
