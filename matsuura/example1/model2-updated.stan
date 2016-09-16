data {
   int N;
   real T[N];
   real Y[N];
   int N_new;
   real T_new[N_new];
}

parameters {
   real a;
   real b;
   real<lower=0> sigma;
}

transformed parameters {
   real Y_hidden[N];
   for (i in 1:N)
      Y_hidden[i] = a + b*T[i];
}

model {
   for (i in 1:N)
      Y[i] ~ normal(Y_hidden[i], sigma);
}

generated quantities {
   real Y_new[N_new];
   for (i in 1:N_new)
      Y_new[i] = normal_rng(a + b*T_new[i], sigma);
}
