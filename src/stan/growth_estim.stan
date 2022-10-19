Model1 = "
functions{
real gompertz_fn(int t, real K, real N0, real a, int k) {
real fn;
fn = K*exp(-log(K/N0)*exp(-a*(t-k)));
return fn;
}
}

data {
// number of week
int T;
int t[T];
int it[T];
int TT;
int N_pop;
}

transformed data{
real iit[T-1];
for(s in 2:T)
iit[s-1] = it[s] - it[s-1];
}

parameters{
real<lower=0> K;
real<lower=0> N0;
real<lower=0> a;
real<lower=0> sigma;
}

transformed parameters{
real gompertz_dif[T];
for(u in 1:T)
gompertz_dif[u] = gompertz_fn(t[u],K,N0,a,t[1]) - it[u];
}

model{ 
a ~ normal(0,1);
K ~ normal(0,0.05*N_pop/2);
for(v in 1:T)
target += normal_lpdf(it[v] | gompertz_fn(t[v],K,N0,a,t[1]), sigma);
}

generated quantities{
real cases[TT];
real r[TT];

for(s in 1:TT){
cases[s] = gompertz_fn(s,K,N0,a,t[1]);
r[s] = a*(log(K/N0)*exp(-a*(s-t[1]))-1);
}
}
"