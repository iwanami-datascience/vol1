data{
  int <lower=0>  N;       // number of years
  real <lower=0,upper=10> y[N];
}

parameters{
  real <lower=0,upper=10> alpha[N];  //state
  real <lower=0> sigma[2];
}

model{
  real tau[2];

  for(i in 1:2)
    tau[i]<-1/(sigma[i]*sigma[i]);

//system model
  for(i in 2:N)
    alpha[i]~normal(alpha[i-1],tau[2]);

//observation model
  for(i in 1:N)
    y[i]~normal(alpha[i],tau[1]);

}