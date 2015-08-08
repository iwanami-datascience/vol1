data {
   int N;
   real T[N];
   real Y[N];
}

parameters {
   real a;
   real b;
   real<lower=0> sigma;
}

model {
   for (i in 1:N){
      Y[i] ~ normal(a + b*T[i], sigma);
   }
   a ~ normal(0, 100);
   b ~ normal(0, 100);
   sigma ~ uniform(0, 1000);
}
