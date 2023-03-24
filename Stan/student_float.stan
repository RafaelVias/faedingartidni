functions {
#include gpbasisfun_functions.stan
}
data {
  int<lower=1> N;                 // number of observations
  vector[N] x;                    // univariate covariate
  vector[N] y;                    // target variable
  int day_of_week[N];             // 
  int day_of_year[N];             // 
  int fostudagurinn_langi[32];    // hatidardagar
  int paskadagur[32];
  int hvitasunna[32];
  int skirdagur[32];
  int laug_f_paska[32];
  int annar_i_paskum[32];
  int sumard_fyrsti[32];
  int uppstigningardagur[32];
  int annar_i_hvitasunnu[32];
  int frid_verzlunarm[32];
        
  real<lower=0> c_f1;  // factor c to determine the boundary value L
  int<lower=1> M_f1;   // number of basis functions for smooth function
  int<lower=1> J_f2;   // number of cos and sin functions for periodic
}
transformed data {
  // Normalize data
  real xmean = mean(x);
  real ymean = mean(y);
  real xsd = sd(x);
  real ysd = sd(y);
  vector[N] xn = (x - xmean)/xsd;
  vector[N] yn = (y - ymean)/ysd;
  // Basis functions for f1
  real L_f1 = c_f1*max(xn);
  matrix[N,M_f1] PHI_f1 = PHI(N, M_f1, L_f1, xn);
  // Basis functions for f2
  real period_year = 365.25/xsd;
  matrix[N,2*J_f2] PHI_f2 = PHI_periodic(N, J_f2, 2*pi()/period_year, xn);
  // Concatenated basis functions for f1 and f2
  matrix[N,M_f1+2*J_f2] PHI_f = append_col(PHI_f1, PHI_f2);
}
parameters {
  vector[M_f1] beta_f1;         // the basis functions coefficients for f1
  vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
  vector[41] beta_f3;           // day of week effect  
  vector[366] beta_f4;          // day of year effect
  vector[10] beta_f5;           // floating special days effects  // didn't have <upper=0>
  real<lower=0> lengthscale_f1; //
  real<lower=0> lengthscale_f2; //
  real<lower=0> sigma_f1;       // scale of f1
  real<lower=0> sigma_f2;       // scale of f2
  real<lower=0> sigma_f4;       // scale of day of year effect
  real<lower=0> sigma_f5;       // scale of day of year effect
  real<lower=0> nu_f4;
  real<lower=0> sigma;          // residual scale
}
model {
  // spectral densities for f1 and f2
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  // day of week and day of year effects
  vector[42] f_day_of_week = append_row(0, beta_f3);
  vector[N] intercept = f_day_of_week[day_of_week] + beta_f4[day_of_year];
  // these floating days overrule day_of_week and day_of_year
  intercept[fostudagurinn_langi] = rep_vector(beta_f5[1], size(fostudagurinn_langi));
  intercept[paskadagur] = rep_vector(beta_f5[2], size(paskadagur));
  intercept[hvitasunna] = rep_vector(beta_f5[3], size(hvitasunna));
  intercept[skirdagur] = rep_vector(beta_f5[4], size(skirdagur));
  intercept[laug_f_paska] = rep_vector(beta_f5[5], size(laug_f_paska));
  intercept[annar_i_paskum] = rep_vector(beta_f5[6], size(annar_i_paskum));
  intercept[sumard_fyrsti] = rep_vector(beta_f5[7], size(sumard_fyrsti));
  intercept[uppstigningardagur] = rep_vector(beta_f5[8], size(uppstigningardagur));
  intercept[annar_i_hvitasunnu] = rep_vector(beta_f5[9], size(annar_i_hvitasunnu));
  intercept[frid_verzlunarm] = rep_vector(beta_f5[10], size(frid_verzlunarm));
  intercept += 0.0;
  // priors
  beta_f1 ~ normal(0, 1);
  beta_f2 ~ normal(0, 1);
  beta_f3 ~ normal(0, 1);
  beta_f4 ~ student_t(nu_f4, 0, sigma_f4);      // double_exponential
  beta_f5 ~ student_t(nu_f4, 0, sigma_f4);      // normal(0, 1)
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  lengthscale_f2 ~ normal(0, .1); 
  sigma_f1 ~ normal(0, 1);
  sigma_f2 ~ normal(0, 1);
  sigma_f4 ~ lognormal(log(1),.5);             // lognormal(log(1),.5);
  sigma_f5 ~ normal(0,.1);             // normal; 0,0.1 
  nu_f4 ~ gamma(2, 1);                          // 2, 0.1
  sigma ~ normal(0, 0.5);  
  // model
  yn ~ normal_id_glm(PHI_f,
		                 intercept,
		                 append_row(diagSPD_f1 .* beta_f1, diagSPD_f2 .* beta_f2),
		                 sigma);
}
generated quantities {
  vector[N] f1;
  vector[N] f2;
  vector[N] f;
  vector[42] f_day_of_week;
  vector[N] log_lik;
  {
    // spectral densities for f1, and f2
    vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
    vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
    // day of week and day of year effects
    vector[42] f_day_of_week_n = append_row(0, beta_f3);
    vector[N] intercept = 0.0 + f_day_of_week_n[day_of_week] + beta_f4[day_of_year];
    // these floating days overrule day_of_week and day_of_year
    intercept[fostudagurinn_langi] = rep_vector(beta_f5[1], size(fostudagurinn_langi));
    intercept[paskadagur] = rep_vector(beta_f5[2], size(paskadagur));
    intercept[hvitasunna] = rep_vector(beta_f5[3], size(hvitasunna));
    intercept[skirdagur] = rep_vector(beta_f5[4], size(skirdagur));
    intercept[laug_f_paska] = rep_vector(beta_f5[5], size(laug_f_paska));
    intercept[annar_i_paskum] = rep_vector(beta_f5[6], size(annar_i_paskum));
    intercept[sumard_fyrsti] = rep_vector(beta_f5[7], size(sumard_fyrsti));
    intercept[uppstigningardagur] = rep_vector(beta_f5[8], size(uppstigningardagur));
    intercept[annar_i_hvitasunnu] = rep_vector(beta_f5[9], size(annar_i_hvitasunnu));
    intercept[frid_verzlunarm] = rep_vector(beta_f5[10], size(frid_verzlunarm));
    intercept += 0.0;
    // functions scaled back to original scale
    f_day_of_week = f_day_of_week_n*ysd;
    f1 = (PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd;
    f2 = (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
    f = f1 + f2 + intercept*ysd + ymean;
    // log_liks for loo
    for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
  }
}
