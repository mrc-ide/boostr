p_s <- 1 - exp(-rs[dix] * dt)
p_l <- 1 - exp(-rl[dix] * dt)
n_s <- Binomial(S, p_s)
n_l <- Binomial(L, p_l)

update(S) <- if(time == dose_timesteps[dix]) 1 else S - n_s
update(L) <- if(time == dose_timesteps[dix]) 1 else L - n_l
update(ps) <- prop_short[dix]
update(it) <- if(time == dose_timesteps[dix]) max(init_titres[dix], it * (ps * S + (1 - ps) * L)) else it
update(ab) <- it * (ps * S + (1 - ps) * L)

initial(S) <- 0
initial(L) <- 0
initial(ab) <- init_titres[1]
initial(ps) <- prop_short[1]
initial(it) <- init_titres[1]

dix <- interpolate(dose_timesteps, dose_index, "constant")

rs <- parameter()
dim(rs) <- parameter(rank = 1)
rl <- parameter()
dim(rl) <- parameter(rank = 1)
dose_timesteps = parameter()
dim(dose_timesteps) <- parameter(rank = 1)
prop_short <- parameter()
dim(prop_short) <- parameter(rank = 1)
init_titres <- parameter()
dim(init_titres) <- parameter(rank = 1)
dose_index <- parameter()
dim(dose_index) <- parameter(rank = 1)
