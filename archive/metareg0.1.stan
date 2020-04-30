/**
 Meta-regression analysis v.0.1 (just testing with three trials)
 
OK with binomial for small numbers but it doesn't seem to like it when success variable is above 904
 
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
   real a_1; //intercept trial 1
   real a_2; //intercept trial 2
   real a_3; //intercept trial 3
   real a_4; //intercept trial 4
   real a_5; //intercept trial 5
   real a_6; //intercept trial 5
   real b; //hand hygiene slope
   real c; //mask slope
}

transformed parameters {
 // vector[J] theta;
 //  theta = mu + tau * eta;
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
 
 
 real pi1_1;  // prob of outcome over followup period
 real pi1_2;
 real pi1_3;
 
 real pi2_1;
 real pi2_2;
 real pi2_3;
 
 real pi5_1;  // prob of outcome over followup period
 real pi5_2;
 real pi5_3;
 
 p1_1 = inv_logit(a_1 + b*hhfreq[1,1] + c*maskhrs[1,1] );
 p1_2 = inv_logit(a_1 + b*hhfreq[1,2] + c*maskhrs[1,2] );
 p1_3 = inv_logit(a_1 + b*hhfreq[1,3] + c*maskhrs[1,3] );
 
 pi1_1 = 1-(1-p1_1)^followupdays[1];
 pi1_2 = 1-(1-p1_2)^followupdays[1];
 pi1_3 = 1-(1-p1_3)^followupdays[1];
 
 p2_1 = inv_logit(a_2 + b*hhfreq[2,1] + c*maskhrs[2,1] );
 p2_2 = inv_logit(a_2 + b*hhfreq[2,2] + c*maskhrs[2,2] );
 p2_3 = inv_logit(a_2 + b*hhfreq[2,3] + c*maskhrs[2,3] );
 
 pi2_1 = 1-(1-p2_1)^followupdays[2];
 pi2_2 = 1-(1-p2_2)^followupdays[2];
 pi2_3 = 1-(1-p2_3)^followupdays[2];
 
 p3_1 = exp(a_3 + b*hhfreq[3,1] + c*maskhrs[3,1] );
 p3_2 = exp(a_3 + b*hhfreq[3,2] + c*maskhrs[3,2] );
 p3_3 = exp(a_3 + b*hhfreq[3,3] + c*maskhrs[3,3] );
 
 p4_1 = exp(a_4 + b*hhfreq[4,1] + c*maskhrs[4,1] );
 p4_2 = exp(a_4 + b*hhfreq[4,2] + c*maskhrs[4,2] );
 
 p5_1 = inv_logit(a_5 + b*hhfreq[5,1] + c*maskhrs[5,1] );
 p5_2 = inv_logit(a_5 + b*hhfreq[5,2] + c*maskhrs[5,2] );
 p5_3 = inv_logit(a_5 + b*hhfreq[5,3] + c*maskhrs[5,3] );
 
 pi5_1 = 1-(1-p5_1)^followupdays[5];
 pi5_2 = 1-(1-p5_2)^followupdays[5];
 pi5_3 = 1-(1-p5_3)^followupdays[5];
 
 //p6_1 = exp(a_6 + b*hhfreq[6,1] + c*maskhrs[6,1] );
 //p6_2 = exp(a_6 + b*hhfreq[6,2] + c*maskhrs[6,2] );
 //p6_3 = exp(a_6 + b*hhfreq[6,3] + c*maskhrs[6,3] );
 
 p6_1 = exp(a_6 + b*hhfreq[6,1]);
 p6_2 = exp(a_6 + b*hhfreq[6,2]  );
 p6_3 = exp(a_6 + b*hhfreq[6,3] );
 
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

   a_1 ~ normal(0, sqrt(1E5));
   a_2 ~ normal(0, sqrt(1E5));
   a_3 ~ normal(0, sqrt(1E5));
   a_4 ~ normal(0, sqrt(1E5));
   a_5 ~ normal(0, sqrt(1E5));
   a_6 ~ normal(0, sqrt(1E5));

   
   b ~ normal(0, 1);
   c ~ normal(0, 1);
}


 