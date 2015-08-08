data {
   int<lower=1> Ni;
   int<lower=1> Nj;
   real Y[Ni,Nj];
   int<lower=1> T;
   int<lower=1, upper=T> T_index[Ni,Nj];
}

parameters {
   real r[Ni,Nj];
   real<lower=0> s_r;
   real beta[T];
   real<lower=0> s_beta;
   real<lower=0> s_y;
}

model {
   for (i in 1:Ni)
      for (j in 3:Nj)
         increment_log_prob(
            normal_log(r[i,j], 2*r[i,j-1] - r[i,j-2], s_r)
         );
   for (j in 1:Nj)
      for (i in 3:Ni)
         increment_log_prob(
            normal_log(r[i,j], 2*r[i-1,j] - r[i-2,j], s_r)
         );
   for (t in 1:T)
      beta[t] ~ normal(0, s_beta);
   for (i in 1:Ni)
      for (j in 1:Nj)
         Y[i,j] ~ normal(r[i,j] + beta[T_index[i,j]], s_y);
}
