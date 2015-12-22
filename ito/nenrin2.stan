data{
  int<lower=0>  N;       // number of years
  real y[N];
}

parameters{
  real <lower=0,upper=10> alpha[N];  //state
  real <lower=0> sigma[2];
}

model{
  real tau[2];
  real mu[N];

  for(i in 1:2)
    tau[i]<-1/(sigma[i]*sigma[i]);

//system model
  for (i in 3:N) {
    mu[i] <- 2 * alpha[i-1] - alpha[i-2];
    alpha[i] ~ normal(mu[i], tau[2]);
  }

//observation model
  for(i in 1:N)
    y[i]~normal(alpha[i],tau[1]);

}