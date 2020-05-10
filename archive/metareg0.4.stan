/**
  Meta-regression analysis v.0.4
  Changes from v0.3 is that we use a log binomial model instead of  logistic model for the binary outcomes
  


***/
  data {
    int<lower=0> T;          // number of trials 
    int<lower=0, upper=3> arms[T];               // number of arms
    int<lower=0, upper=1> binaryoutcome[T];  // 1 if outcome type is binary (trial participatnts infected or not , 0 otherwise meaning that people can be infected more than once)
    int cases[T,3];  // number of ARI cases
    int denoms[T,3];  // element [i,j] gives denominators for trial i arm j
    int followupdays[T]; //number of days individuals were followed up for
    real hhfreq[T,3];  //hand hygiene frequency is for trial i arm j
    real maskhrs[T,3];  //daily mask hours for trial i arm j
    int pdaysatrisk_arm1[T]; //person days at risk in the control arm (arm 1)
    int pdaysatrisk_arm2[T]; //person days at risk in arm 2
    int pdaysatrisk_arm3[T]; //person days at risk in arm 3
  }

parameters {
  vector[6] a; //intercepts
  vector[6] b; //slopes hand hygiene
  vector[6] c; //slopes mask
  real a0; //mean intercept
  real b0; //mean hand hygiene slope
  real c0; //mask slope
  real<lower=0> sigmasq_a; //intercept variance 
  real<lower=0> sigmasq_b; //slope variance hand hygiene
  real<lower=0> sigmasq_c; //slope variance  mask use
}

transformed parameters {
  // vector[J] theta;
  //  theta = mu + tau * eta;
  real sigma_a = sqrt(sigmasq_a);
  real sigma_b = sqrt(sigmasq_b);
  real sigma_c = sqrt(sigmasq_c);
  real p1_1;  //trial one arm 1    prob of outcome per day
  real p1_2;  //trial one arm 2
  real p1_3;  //trial one arm 3
  real p2_1;  //trial two arm 1
  real p2_2;  //trial two arm 2
  real p2_3;  //trial two arm 3
  real p3_1;  //trial three arm 1
  real p3_2;  //trial three arm 2
  real p3_3;  //trial three arm 3
  real p4_1;  //trial four arm 1
  real p4_2;  //trial four arm 2
  real p5_1;  //trial five arm 1
  real p5_2;  //trial five arm 2
  real p5_3;  //trial five arm 3
  real p6_1;  //trial six arm 1
  real p6_2;  //trial six arm 2
  real p6_3;  //trial six arm 3
  
  real RR_handwashing;  
  real RR_masks;  
  
  real pi1_1;  // prob of outcome over followup period
  real pi1_2;
  real pi1_3;
  
  real pi2_1;
  real pi2_2;
  real pi2_3;
  
  real pi5_1;  // prob of outcome over followup period
  real pi5_2;
  real pi5_3;
  
  RR_handwashing = exp(b0);  
  RR_masks=exp(c0);  
  
  p1_1 = exp(a[1] + b[1]*hhfreq[1,1] + c[1]*maskhrs[1,1] );
  p1_2 = exp(a[1] + b[1]*hhfreq[1,2] + c[1]*maskhrs[1,2] );
  p1_3 = exp(a[1] + b[1]*hhfreq[1,3] + c[1]*maskhrs[1,3] );
  
  pi1_1 = 1-(1-p1_1)^followupdays[1];
  pi1_2 = 1-(1-p1_2)^followupdays[1];
  pi1_3 = 1-(1-p1_3)^followupdays[1];
  
  p2_1 = exp(a[2] + b[2]*hhfreq[2,1] + c[2]*maskhrs[2,1] );
  p2_2 = exp(a[2] + b[2]*hhfreq[2,2] + c[2]*maskhrs[2,2] );
  p2_3 = exp(a[2] + b[2]*hhfreq[2,3] + c[2]*maskhrs[2,3] );
  
  pi2_1 = 1-(1-p2_1)^followupdays[2];
  pi2_2 = 1-(1-p2_2)^followupdays[2];
  pi2_3 = 1-(1-p2_3)^followupdays[2];
  
  p3_1 = exp(a[3] + b[3]*hhfreq[3,1] + c[3]*maskhrs[3,1] );
  p3_2 = exp(a[3] + b[3]*hhfreq[3,2] + c[3]*maskhrs[3,2] );
  p3_3 = exp(a[3] + b[3]*hhfreq[3,3] + c[3]*maskhrs[3,3] );
  
  p4_1 = exp(a[4] + b[4]*hhfreq[4,1] + c[4]*maskhrs[4,1] );
  p4_2 = exp(a[4] + b[4]*hhfreq[4,2] + c[4]*maskhrs[4,2] );
  
//  p5_1 = inv_logit(a[5] + b[5]*hhfreq[5,1] + c[5]*maskhrs[5,1] );
//  p5_2 = inv_logit(a[5] + b[5]*hhfreq[5,2] + c[5]*maskhrs[5,2] );
//  p5_3 = inv_logit(a[5] + b[5]*hhfreq[5,3] + c[5]*maskhrs[5,3] );

  p5_1 = exp(a[5] + b[5]*hhfreq[5,1] + c[5]*maskhrs[5,1] );
  p5_2 = exp(a[5] + b[5]*hhfreq[5,2] + c[5]*maskhrs[5,2] );
  p5_3 = exp(a[5] + b[5]*hhfreq[5,3] + c[5]*maskhrs[5,3] );
  
    
  pi5_1 = 1-(1-p5_1)^followupdays[5];
  pi5_2 = 1-(1-p5_2)^followupdays[5];
  pi5_3 = 1-(1-p5_3)^followupdays[5];
  
  //p6_1 = exp(a_6 + b[6]*hhfreq[6,1] + c*maskhrs[6,1] );
  //p6_2 = exp(a_6 + b[6]*hhfreq[6,2] + c*maskhrs[6,2] );
  //p6_3 = exp(a_6 + b[6]*hhfreq[6,3] + c*maskhrs[6,3] );
  
  p6_1 = exp(a[6] + b[6]*hhfreq[6,1]);
  p6_2 = exp(a[6] + b[6]*hhfreq[6,2]  );
  p6_3 = exp(a[6] + b[6]*hhfreq[6,3] );
  
}

model { 
  //cases[1,1] ~ binomial_logit(denoms[1,1], a + b*hhfreq[1,1]);
  //cases[1,2] ~ binomial_logit(denoms[1,2], a + b*hhfreq[1,2]);
  cases[1,1] ~ binomial(denoms[1,1], pi1_1);
  cases[1,2] ~ binomial(denoms[1,2], pi1_2);
  cases[1,3] ~ binomial(denoms[1,3], pi1_3);
  
  cases[2,1] ~ binomial(denoms[2,1], pi2_1);
  cases[2,2] ~ binomial(denoms[2,2], pi2_2);
  cases[2,3] ~ binomial(denoms[2,3], pi2_3);
  
  cases[3,1] ~ poisson(denoms[3,1]*p3_1);
  cases[3,2] ~ poisson(denoms[3,2]*p3_2);
  cases[3,3] ~ poisson(denoms[3,3]*p3_3);
  
  cases[4,1] ~ poisson(denoms[4,1]*p4_1);
  cases[4,2] ~ poisson(denoms[4,2]*p4_2);
  
  cases[5,1] ~ binomial(denoms[5,1],pi5_1);
  cases[5,2] ~ binomial(denoms[5,2], pi5_2);//
    cases[5,3] ~ binomial(denoms[5,3], pi5_3);
  
  cases[6,1] ~ poisson(denoms[6,1]*p6_1);
  cases[6,2] ~ poisson(denoms[6,2]*p6_2);
  cases[6,3] ~ poisson(denoms[6,3]*p6_3);
  
  a0 ~ normal(0, 1);
  b0 ~ normal(0, 1);
  c0 ~ normal(0, 1);
  //sigmasq_a ~ inv_gamma(.001, .001);
  
  a ~ normal(a0, sigmasq_a );
  b ~ normal(b0, sigmasq_b );
  c ~ normal(c0, sigmasq_c );
  
  sigmasq_a ~ cauchy(0, 5); // half-cauchy prior
  sigmasq_b ~ cauchy(0, 5); // half-cauchy prior
  sigmasq_c ~ cauchy(0, 5); // half-cauchy prior
  
}


