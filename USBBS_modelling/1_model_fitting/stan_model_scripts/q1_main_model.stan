data {
  // number of observations
  int<lower=0> N; 
  
  // Response variables
  vector[N] q1_t2; 
  
  ////////////////////////
  // Explanatory variables
  
  // Static-equilibrium vars
  vector[N] urban_t2;  
    vector[N] urban_t1; 
  vector[N] forest_t2;  
    vector[N] forest_t1;
  vector[N] grass_t2;  
    vector[N] grass_t1;
  vector[N] crop_t2;  
    vector[N] crop_t1;
  vector[N] wet_t2;  
    vector[N] wet_t1;
  vector[N] lc_q1_t2;  
    //vector[N] lc_q1_t1;
  
  // quadratic terms vars
  vector[N] urban_squared_t2;
  vector[N] forest_squared_t2;
  vector[N] crop_squared_t2;
  vector[N] grass_squared_t2;
  vector[N] wet_squared_t2;
  //vector[N] lc_q1_squared_t2;
    vector[N] urban_squared_t1;
    vector[N] forest_squared_t1;
    vector[N] crop_squared_t1;
    vector[N] grass_squared_t1;
    vector[N] wet_squared_t1;
    //vector[N] lc_q1_squared_t1;

  // Change-delay vars
  vector[N] delta_pos_urban;  
  vector[N] delta_pos_forest;  
  vector[N] delta_pos_grass;  
  vector[N] delta_pos_crop;  
  vector[N] delta_pos_wet;  
  //vector[N] delta_pos_lc_q1; 
  //vector[N] delta_neg_urban;  
  vector[N] delta_neg_forest;  
  vector[N] delta_neg_grass;  
  vector[N] delta_neg_crop;  
  vector[N] delta_neg_wet;  
  //vector[N] delta_neg_lc_q1;
  
  // additional vars
  vector[N] time; // survey starting time
  vector[N] temp_t2; // temp
  vector[N] temp_squared_t2; // temp
    
  int<lower=1> O; // number of observer levels for random effect
  int<lower=1, upper=O> observerID[N];
  
  //int<lower=1> R; // number of route levels for random effect
  //int<lower=1, upper=R> routeID[N];
  
}

parameters {
  
  real<lower=0> sigma;
  
  real<lower=0> intercept;
  
  // linear land cover parameters
  real<lower=0> b_urban;
  real<lower=0> b_forest;
  real<lower=0> b_grass;
  real<lower=0> b_crop;
  real<lower=0> b_wet;
  
  // quadratic land cover parameters
  real<upper=0> b2_urban;
  real<upper=0> b2_forest;
  real<upper=0> b2_grass;
  real<upper=0> b2_crop;
  real<upper=0> b2_wet;
  
  // linear*linear land cover interaction
  real b_urban_forest;  
  real b_urban_grass; 
  real b_urban_crop; 
  real b_forest_grass; 
  real b_forest_crop; 
  real b_grass_crop; 
  real b_wet_urban; 
  real b_wet_forest; 
  real b_wet_grass;
  real b_wet_crop;
  
  // linear*quadratic land cover interaction
  real b_urban2_forest;
  real b_urban2_grass;
  real b_urban2_wet;
  real b_urban2_crop;
  real b_forest2_urban;
  real b_forest2_crop;
  real b_forest2_grass;
  real b_forest2_wet;
  real b_grass2_urban;
  real b_grass2_forest;
  real b_grass2_crop;
  real b_grass2_wet;
  real b_crop2_urban;
  real b_crop2_forest;
  real b_crop2_grass;
  real b_crop2_wet;
  real b_wet2_urban;
  real b_wet2_forest;
  real b_wet2_grass;
  real b_wet2_crop;

  // delay land cover parameters <lower=0, upper=1>
  real<lower=0, upper=1> c_pos_urban;
  real<lower=0, upper=1> c_pos_forest;
  real<lower=0, upper=1> c_pos_grass;
  real<lower=0, upper=1> c_pos_crop;
  real<lower=0, upper=1> c_pos_wet;
  //real<lower=0, upper=1> c_pos_lc_q1;
    //real<lower=0, upper=1> c_neg_urban;
    real<lower=0, upper=1> c_neg_forest;
    real<lower=0, upper=1> c_neg_grass;
    real<lower=0, upper=1> c_neg_crop;
    real<lower=0, upper=1> c_neg_wet;
    //real<lower=0, upper=1> c_neg_lc_q1;

  // additional params
  real b_time;
  real<lower=0> b_temp;
  real<upper=0> b2_temp;
  real<lower=0> b_lc_q1;
  //real b2_lc_q1;
  
  // random effects variance
  real<lower=0> sigma_observer;
  //real<lower=0> sigma_route;
  
  //vector[R] random_route; // random effect vectors
  vector[O] random_observer; // random effect vectors
}

transformed parameters {
 
  // main model
  vector[N] mu;

  mu = intercept + 
  
  // equilibrium model t2
   // linear and quadratic terms
                   (b_urban * urban_t2 + b2_urban * urban_squared_t2 + 
                    b_forest * forest_t2 + b2_forest * forest_squared_t2 + 
                    b_grass * grass_t2 + b2_grass * grass_squared_t2 +
                    b_crop * crop_t2 + b2_crop * crop_squared_t2 +
                    b_wet * wet_t2 + b2_wet * wet_squared_t2 +
                    
   // # linear*linear interactions t2
                    b_urban_forest * (urban_t2 .* forest_t2) + 
                    b_urban_forest * (urban_t2 .* forest_t2) + 
                    b_urban_grass * (urban_t2 .* grass_t2) +        
                    b_urban_crop* (urban_t2 .* crop_t2) +          
                    b_forest_grass * (forest_t2 .* grass_t2) +      
                    b_forest_crop * (forest_t2 .* crop_t2) +        
                    b_grass_crop * (grass_t2 .* crop_t2) +  
                    b_wet_urban * (wet_t2 .* urban_t2) +
                    b_wet_forest * (wet_t2 .* forest_t2) +
                    b_wet_grass * (wet_t2 .* grass_t2) +
                    b_wet_crop * (wet_t2 .* crop_t2) +
                    
  // # linear*quadratic interactions t2
                    b_urban2_forest * (urban_squared_t2 .*  forest_t2) +               
                    b_urban2_grass * (urban_squared_t2 .* grass_t2) +        
                    b_urban2_crop * (urban_squared_t2 .* crop_t2) + 
                    b_urban2_wet * (urban_squared_t2 .* wet_t2) +
                    b_forest2_urban * (forest_squared_t2 .* urban_t2) + 
                    b_forest2_crop * (forest_squared_t2 .* crop_t2) +
                    b_forest2_grass * (forest_squared_t2 .* grass_t2) + 
                    b_forest2_wet * (forest_squared_t2 .* wet_t2) +
                    b_grass2_urban * (grass_squared_t2 .* urban_t2) + 
                    b_grass2_forest * (grass_squared_t2 .* forest_t2) +
                    b_grass2_crop * (grass_squared_t2 .* crop_t2) + 
                    b_grass2_wet * (grass_squared_t2 .* wet_t2) +
                    b_crop2_urban * (crop_squared_t2 .* urban_t2) + 
                    b_crop2_forest * (crop_squared_t2 .* forest_t2) +
                    b_crop2_grass * (crop_squared_t2 .* grass_t2) + 
                    b_crop2_wet * (crop_squared_t2 .* wet_t2) +
                    b_wet2_urban * (wet_squared_t2 .* urban_t2) + 
                    b_wet2_forest * (wet_squared_t2 .* forest_t2) +
                    b_wet2_grass * (wet_squared_t2 .* grass_t2) + 
                    b_wet2_crop * (wet_squared_t2 .* crop_t2)) .*
                    
  // delay model t2    
             (exp((-c_pos_urban * delta_pos_urban) +   
                  (-c_pos_forest * delta_pos_forest) + (-c_neg_forest * delta_neg_forest) +  
                  (-c_pos_grass * delta_pos_grass) + (-c_neg_grass * delta_neg_grass) +
                  (-c_pos_crop * delta_pos_crop) + (-c_neg_crop * delta_neg_crop) +
                  (-c_pos_wet * delta_pos_wet) + (-c_neg_wet * delta_neg_wet))) +
                  //(-c_pos_lc_q1 * delta_pos_lc_q1) + (-c_neg_lc_q1 * delta_neg_lc_q1))) + 
                    
  // equilibrium model t1
     // linear and quadratic terms
                   (b_urban * urban_t1 + b2_urban * urban_squared_t1 + 
                    b_forest * forest_t1 + b2_forest * forest_squared_t1 +
                    b_grass * grass_t1 + b2_grass * grass_squared_t1 +
                    b_crop * crop_t1 + b2_crop * crop_squared_t1 +
                    b_wet * wet_t1 + b2_wet * wet_squared_t1 +
                    
  // # linear*linear interactions t1
                    b_urban_forest * (urban_t1 .* forest_t1) + 
                    b_urban_grass * (urban_t1 .* grass_t1) +        
                    b_urban_crop* (urban_t1 .* crop_t1) +          
                    b_forest_grass * (forest_t1 .* grass_t1) +      
                    b_forest_crop * (forest_t1 .* crop_t1) +        
                    b_grass_crop * (grass_t1 .* crop_t1) +  
                    b_wet_urban * (wet_t1 .* urban_t1) +
                    b_wet_forest * (wet_t1 .* forest_t1) +
                    b_wet_grass * (wet_t1 .* grass_t1) +
                    b_wet_crop * (wet_t1 .* crop_t1) + 

  // # linear*quadratic interactions t1
                    b_urban2_forest * (urban_squared_t1 .*  forest_t1) +               
                    b_urban2_grass * (urban_squared_t1 .* grass_t1) +        
                    b_urban2_crop * (urban_squared_t1 .* crop_t1) + 
                    b_urban2_wet * (urban_squared_t1 .* wet_t1) +
                    b_forest2_urban * (forest_squared_t1 .* urban_t1) + 
                    b_forest2_crop * (forest_squared_t1 .* crop_t1) +
                    b_forest2_grass * (forest_squared_t1 .* grass_t1) + 
                    b_forest2_wet * (forest_squared_t1 .* wet_t1) +
                    b_grass2_urban * (grass_squared_t1 .* urban_t1) + 
                    b_grass2_forest * (grass_squared_t1 .* forest_t1) +
                    b_grass2_crop * (grass_squared_t1 .* crop_t1) + 
                    b_grass2_wet * (grass_squared_t1 .* wet_t1) +
                    b_crop2_urban * (crop_squared_t1 .* urban_t1) + 
                    b_crop2_forest * (crop_squared_t1 .* forest_t1) +
                    b_crop2_grass * (crop_squared_t1 .* grass_t1) + 
                    b_crop2_wet * (crop_squared_t1 .* wet_t1) +
                    b_wet2_urban * (wet_squared_t1 .* urban_t1) + 
                    b_wet2_forest * (wet_squared_t1 .* forest_t1) +
                    b_wet2_grass * (wet_squared_t1 .* grass_t1) + 
                    b_wet2_crop * (wet_squared_t1 .* crop_t1)) .* 
                                                
  // delay model t1                    
         (1 - exp((-c_pos_urban * delta_pos_urban) +   
                  (-c_pos_forest * delta_pos_forest) + (-c_neg_forest * delta_neg_forest) +  
                  (-c_pos_grass * delta_pos_grass) + (-c_neg_grass * delta_neg_grass) +
                  (-c_pos_crop * delta_pos_crop) + (-c_neg_crop * delta_neg_crop) +
                  (-c_pos_wet * delta_pos_wet) + (-c_neg_wet * delta_neg_wet))) +
                    //(-c_pos_lc_q1 * delta_pos_lc_q1) + (-c_neg_lc_q1 * delta_neg_lc_q1))) +
  
  // additional terms                  
                  b_time * time + 
                  b_lc_q1 * lc_q1_t2 + //b2_lc_q1 * lc_q1_squared_t2 +
                  b_temp * temp_t2 + b2_temp * temp_squared_t2 +
                  //random_route[routeID] +
                  random_observer[observerID];
                  
}

model {

  // LIKELIHOOD
  q1_t2 ~ normal(mu, sigma);
  
  // PRIORS
  // equilibrium parameters
  
  // linear params
  b_urban ~ normal(0,1);
  b_forest ~ normal(0,1);
  b_grass ~ normal(0,1);
  b_crop ~ normal(0,1);
  b_wet ~ normal(0,1);

  // quadratic params
  b2_urban ~ normal(0,1);
  b2_forest ~ normal(0,1);
  b2_grass ~ normal(0,1);
  b2_crop ~ normal(0,1);
  b2_wet ~ normal(0,1);

  // linear*linear land cover interaction
  b_urban_forest ~ normal(0,1);  
  b_urban_grass ~ normal(0,1); 
  b_urban_crop ~ normal(0,1); 
  b_forest_grass ~ normal(0,1); 
  b_forest_crop ~ normal(0,1); 
  b_grass_crop ~ normal(0,1); 
  b_wet_urban ~ normal(0,1); 
  b_wet_forest ~ normal(0,1); 
  b_wet_grass ~ normal(0,1);
  b_wet_crop ~ normal(0,1);
  
  // linear*quadratic land cover interaction
  b_urban2_forest ~ normal(0,1);
  b_urban2_grass ~ normal(0,1);
  b_urban2_wet ~ normal(0,1);
  b_urban2_crop ~ normal(0,1);
  b_forest2_urban ~ normal(0,1);
  b_forest2_crop ~ normal(0,1);
  b_forest2_grass ~ normal(0,1);
  b_forest2_wet ~ normal(0,1);
  b_grass2_urban ~ normal(0,1);
  b_grass2_forest ~ normal(0,1);
  b_grass2_crop ~ normal(0,1);
  b_grass2_wet ~ normal(0,1);
  b_crop2_urban ~ normal(0,1);
  b_crop2_forest ~ normal(0,1);
  b_crop2_grass ~ normal(0,1);
  b_crop2_wet ~ normal(0,1);
  b_wet2_urban ~ normal(0,1);
  b_wet2_forest ~ normal(0,1);
  b_wet2_grass ~ normal(0,1);
  b_wet2_crop ~ normal(0,1);
  
  // delay parameters, no need to define priors if they bounded in above statement
  //c_pos_urban ~ uniform(0,1);
  //c_pos_forest ~ uniform(0,1);
  //c_pos_grass ~ uniform(0,1);
  //c_pos_crop ~ uniform(0,1);
  //c_pos_wet ~ uniform(0,1);
  //c_pos_lc_q1 ~ uniform(0,1);
  //c_neg_urban ~ uniform(0,1);
  //c_neg_forest ~ uniform(0,1);
  //c_neg_grass ~ uniform(0,1);
  //c_neg_crop ~ uniform(0,1);
  //c_neg_wet ~ uniform(0,1);
  //c_neg_lc_q1 ~ uniform(0,1);
  
  // additional parameters
  b_time ~ normal(0,1);
  b_temp ~ normal(0,1);
  b2_temp ~ normal(0,1);
  b_lc_q1 ~ normal(0,1);
  //b2_lc_q1 ~ normal(0,1);

  sigma_observer ~ gamma(0.001, 0.001); // sd of random effect
  //sigma_route ~ gamma(0.001, 0.001); // sd of random effect
  
  sigma ~ gamma(0.001, 0.001);
  
  random_observer ~ normal(0, sigma_observer);
  //random_route ~ normal(0, sigma_route);

}

generated quantities{
  
 vector[N] log_lik;

  for (n in 1:N) {

   log_lik[n] = normal_lpdf(q1_t2[n] | mu[n], sigma);
    
  } 
  
}

