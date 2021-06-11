
#'-----------------------------------------------------------------------------------------------------
#'
#' * EQUILIBRIUM FUNCTION, only equilibrium at timepoint 2 without accounting for past landscape *
#'

equilibrium_function <- function(data_list=NA, param_list=NA) { 
  
  predicted <- 
    
    param_list$intercept +

    # linear and quadratic terms
    param_list$b_urban * data_list$urban_t2 + param_list$b2_urban * data_list$urban_squared_t2 + 
    param_list$b_forest * data_list$forest_t2 + param_list$b2_forest * data_list$forest_squared_t2 + 
    param_list$b_grass * data_list$grass_t2 + param_list$b2_grass * data_list$grass_squared_t2 +
    param_list$b_crop * data_list$crop_t2 + param_list$b2_crop * data_list$crop_squared_t2 +
    param_list$b_wet * data_list$wet_t2 + param_list$b2_wet * data_list$wet_squared_t2 +
      
    # linear*linear interactions t2
    param_list$b_urban_forest * (data_list$urban_t2 * data_list$forest_t2) + 
    param_list$b_urban_forest * (data_list$urban_t2 * data_list$forest_t2) + 
    param_list$b_urban_grass * (data_list$urban_t2 * data_list$grass_t2) +        
    param_list$b_urban_crop* (data_list$urban_t2 * data_list$crop_t2) +          
    param_list$b_forest_grass * (data_list$forest_t2 * data_list$grass_t2) +      
    param_list$b_forest_crop * (data_list$forest_t2 * data_list$crop_t2) +        
    param_list$b_grass_crop * (data_list$grass_t2 * data_list$crop_t2) +  
    param_list$b_wet_urban * (data_list$wet_t2 * data_list$urban_t2) +
    param_list$b_wet_forest * (data_list$wet_t2 * data_list$forest_t2) +
    param_list$b_wet_grass * (data_list$wet_t2 * data_list$grass_t2) +
    param_list$b_wet_crop * (data_list$wet_t2 * data_list$crop_t2) +
      
    # linear*quadratic interactions t2
    param_list$b_urban2_forest * (data_list$urban_squared_t2 *  data_list$forest_t2) +               
    param_list$b_urban2_grass * (data_list$urban_squared_t2 * data_list$grass_t2) +        
    param_list$b_urban2_crop * (data_list$urban_squared_t2 * data_list$crop_t2) + 
    param_list$b_urban2_wet * (data_list$urban_squared_t2 * data_list$wet_t2) +
    param_list$b_forest2_urban * (data_list$forest_squared_t2 * data_list$urban_t2) + 
    param_list$b_forest2_crop * (data_list$forest_squared_t2 * data_list$crop_t2) +
    param_list$b_forest2_grass * (data_list$forest_squared_t2 * data_list$grass_t2) + 
    param_list$b_forest2_wet * (data_list$forest_squared_t2 * data_list$wet_t2) +
    param_list$b_grass2_urban * (data_list$grass_squared_t2 * data_list$urban_t2) + 
    param_list$b_grass2_forest * (data_list$grass_squared_t2 * data_list$forest_t2) +
    param_list$b_grass2_crop * (data_list$grass_squared_t2 * data_list$crop_t2) + 
    param_list$b_grass2_wet * (data_list$grass_squared_t2 * data_list$wet_t2) +
    param_list$b_crop2_urban * (data_list$crop_squared_t2 * data_list$urban_t2) + 
    param_list$b_crop2_forest * (data_list$crop_squared_t2 * data_list$forest_t2) +
    param_list$b_crop2_grass * (data_list$crop_squared_t2 * data_list$grass_t2) + 
    param_list$b_crop2_wet * (data_list$crop_squared_t2 * data_list$wet_t2) +
    param_list$b_wet2_urban * (data_list$wet_squared_t2 * data_list$urban_t2) + 
    param_list$b_wet2_forest * (data_list$wet_squared_t2 * data_list$forest_t2) +
    param_list$b_wet2_grass * (data_list$wet_squared_t2 * data_list$grass_t2) + 
    param_list$b_wet2_crop * (data_list$wet_squared_t2 * data_list$crop_t2) +

    # additional terms                  
    param_list$b_time * data_list$time + 
    param_list$b_lc_q1 * data_list$lc_q1_t2 +
    param_list$b_temp * data_list$temp_t2 + 
    param_list$b2_temp * data_list$temp_squared_t2 # + random_observer[observerID];
            
    return(predicted)
  
}

#'-----------------------------------------------------------------------------------------------------
#'
#' * DELAY FUNCTION, whole model *
#'

delay_function <- function(data_list=NA, param_list=NA) { 
  
  predicted <- 
    
    param_list$intercept +
    
    # linear and quadratic terms t2
  ( param_list$b_urban * data_list$urban_t2 + param_list$b2_urban * data_list$urban_squared_t2 + 
    param_list$b_forest * data_list$forest_t2 + param_list$b2_forest * data_list$forest_squared_t2 + 
    param_list$b_grass * data_list$grass_t2 + param_list$b2_grass * data_list$grass_squared_t2 +
    param_list$b_crop * data_list$crop_t2 + param_list$b2_crop * data_list$crop_squared_t2 +
    param_list$b_wet * data_list$wet_t2 + param_list$b2_wet * data_list$wet_squared_t2 +
    
    # linear*linear interactions t2
    param_list$b_urban_forest * (data_list$urban_t2 * data_list$forest_t2) + 
    param_list$b_urban_forest * (data_list$urban_t2 * data_list$forest_t2) + 
    param_list$b_urban_grass * (data_list$urban_t2 * data_list$grass_t2) +        
    param_list$b_urban_crop* (data_list$urban_t2 * data_list$crop_t2) +          
    param_list$b_forest_grass * (data_list$forest_t2 * data_list$grass_t2) +      
    param_list$b_forest_crop * (data_list$forest_t2 * data_list$crop_t2) +        
    param_list$b_grass_crop * (data_list$grass_t2 * data_list$crop_t2) +  
    param_list$b_wet_urban * (data_list$wet_t2 * data_list$urban_t2) +
    param_list$b_wet_forest * (data_list$wet_t2 * data_list$forest_t2) +
    param_list$b_wet_grass * (data_list$wet_t2 * data_list$grass_t2) +
    param_list$b_wet_crop * (data_list$wet_t2 * data_list$crop_t2) +
    
    # linear*quadratic interactions t2
    param_list$b_urban2_forest * (data_list$urban_squared_t2 *  data_list$forest_t2) +               
    param_list$b_urban2_grass * (data_list$urban_squared_t2 * data_list$grass_t2) +        
    param_list$b_urban2_crop * (data_list$urban_squared_t2 * data_list$crop_t2) + 
    param_list$b_urban2_wet * (data_list$urban_squared_t2 * data_list$wet_t2) +
    param_list$b_forest2_urban * (data_list$forest_squared_t2 * data_list$urban_t2) + 
    param_list$b_forest2_crop * (data_list$forest_squared_t2 * data_list$crop_t2) +
    param_list$b_forest2_grass * (data_list$forest_squared_t2 * data_list$grass_t2) + 
    param_list$b_forest2_wet * (data_list$forest_squared_t2 * data_list$wet_t2) +
    param_list$b_grass2_urban * (data_list$grass_squared_t2 * data_list$urban_t2) + 
    param_list$b_grass2_forest * (data_list$grass_squared_t2 * data_list$forest_t2) +
    param_list$b_grass2_crop * (data_list$grass_squared_t2 * data_list$crop_t2) + 
    param_list$b_grass2_wet * (data_list$grass_squared_t2 * data_list$wet_t2) +
    param_list$b_crop2_urban * (data_list$crop_squared_t2 * data_list$urban_t2) + 
    param_list$b_crop2_forest * (data_list$crop_squared_t2 * data_list$forest_t2) +
    param_list$b_crop2_grass * (data_list$crop_squared_t2 * data_list$grass_t2) + 
    param_list$b_crop2_wet * (data_list$crop_squared_t2 * data_list$wet_t2) +
    param_list$b_wet2_urban * (data_list$wet_squared_t2 * data_list$urban_t2) + 
    param_list$b_wet2_forest * (data_list$wet_squared_t2 * data_list$forest_t2) +
    param_list$b_wet2_grass * (data_list$wet_squared_t2 * data_list$grass_t2) + 
    param_list$b_wet2_crop * (data_list$wet_squared_t2 * data_list$crop_t2)) *
    
    # delay model t2                    
    exp((-param_list$c_pos_urban * data_list$delta_pos_urban) +   
        (-param_list$c_pos_forest * data_list$delta_pos_forest) + (-param_list$c_neg_forest * data_list$delta_neg_forest) +  
        (-param_list$c_pos_grass * data_list$delta_pos_grass) + (-param_list$c_neg_grass * data_list$delta_neg_grass) +
        (-param_list$c_pos_crop * data_list$delta_pos_crop) + (-param_list$c_neg_crop * data_list$delta_neg_crop) +
        (-param_list$c_pos_wet * data_list$delta_pos_wet) + (-param_list$c_neg_wet * data_list$delta_neg_wet)) +
    
    # linear and quadratic terms t1
  ( param_list$b_urban * data_list$urban_t1 + param_list$b2_urban * data_list$urban_squared_t1 + 
    param_list$b_forest * data_list$forest_t1 + param_list$b2_forest * data_list$forest_squared_t1 + 
    param_list$b_grass * data_list$grass_t1 + param_list$b2_grass * data_list$grass_squared_t1 +
    param_list$b_crop * data_list$crop_t1 + param_list$b2_crop * data_list$crop_squared_t1 +
    param_list$b_wet * data_list$wet_t1 + param_list$b2_wet * data_list$wet_squared_t1 +
        
    # linear*linear interactions t1
    param_list$b_urban_forest * (data_list$urban_t1 * data_list$forest_t1) + 
    param_list$b_urban_forest * (data_list$urban_t1 * data_list$forest_t1) + 
    param_list$b_urban_grass * (data_list$urban_t1 * data_list$grass_t1) +        
    param_list$b_urban_crop* (data_list$urban_t1 * data_list$crop_t1) +          
    param_list$b_forest_grass * (data_list$forest_t1 * data_list$grass_t1) +      
    param_list$b_forest_crop * (data_list$forest_t1 * data_list$crop_t1) +        
    param_list$b_grass_crop * (data_list$grass_t1 * data_list$crop_t1) +  
    param_list$b_wet_urban * (data_list$wet_t1 * data_list$urban_t1) +
    param_list$b_wet_forest * (data_list$wet_t1 * data_list$forest_t1) +
    param_list$b_wet_grass * (data_list$wet_t1 * data_list$grass_t1) +
    param_list$b_wet_crop * (data_list$wet_t1 * data_list$crop_t1) +
        
    # linear*quadratic interactions t1
    param_list$b_urban2_forest * (data_list$urban_squared_t1 *  data_list$forest_t1) +               
    param_list$b_urban2_grass * (data_list$urban_squared_t1 * data_list$grass_t1) +        
    param_list$b_urban2_crop * (data_list$urban_squared_t1 * data_list$crop_t1) + 
    param_list$b_urban2_wet * (data_list$urban_squared_t1 * data_list$wet_t1) +
    param_list$b_forest2_urban * (data_list$forest_squared_t1 * data_list$urban_t1) + 
    param_list$b_forest2_crop * (data_list$forest_squared_t1 * data_list$crop_t1) +
    param_list$b_forest2_grass * (data_list$forest_squared_t1 * data_list$grass_t1) + 
    param_list$b_forest2_wet * (data_list$forest_squared_t1 * data_list$wet_t1) +
    param_list$b_grass2_urban * (data_list$grass_squared_t1 * data_list$urban_t1) + 
    param_list$b_grass2_forest * (data_list$grass_squared_t1 * data_list$forest_t1) +
    param_list$b_grass2_crop * (data_list$grass_squared_t1 * data_list$crop_t1) + 
    param_list$b_grass2_wet * (data_list$grass_squared_t1 * data_list$wet_t1) +
    param_list$b_crop2_urban * (data_list$crop_squared_t1 * data_list$urban_t1) + 
    param_list$b_crop2_forest * (data_list$crop_squared_t1 * data_list$forest_t1) +
    param_list$b_crop2_grass * (data_list$crop_squared_t1 * data_list$grass_t1) + 
    param_list$b_crop2_wet * (data_list$crop_squared_t1 * data_list$wet_t1) +
    param_list$b_wet2_urban * (data_list$wet_squared_t1 * data_list$urban_t1) + 
    param_list$b_wet2_forest * (data_list$wet_squared_t1 * data_list$forest_t1) +
    param_list$b_wet2_grass * (data_list$wet_squared_t1 * data_list$grass_t1) + 
    param_list$b_wet2_crop * (data_list$wet_squared_t1 * data_list$crop_t1)) *
    
    # delay model t1                   
    (1 - exp((-param_list$c_pos_urban * data_list$delta_pos_urban) +   
            (-param_list$c_pos_forest * data_list$delta_pos_forest) + (-param_list$c_neg_forest * data_list$delta_neg_forest) +  
            (-param_list$c_pos_grass * data_list$delta_pos_grass) + (-param_list$c_neg_grass * data_list$delta_neg_grass) +
            (-param_list$c_pos_crop * data_list$delta_pos_crop) + (-param_list$c_neg_crop * data_list$delta_neg_crop) +
            (-param_list$c_pos_wet * data_list$delta_pos_wet) + (-param_list$c_neg_wet * data_list$delta_neg_wet))) +
    
    # additional terms                  
    param_list$b_time * data_list$time + 
    param_list$b_lc_q1 * data_list$lc_q1_t2 +
    param_list$b_temp * data_list$temp_t2 + 
    param_list$b2_temp * data_list$temp_squared_t2 # + random_observer[observerID];
  
  return(predicted)
  
}
