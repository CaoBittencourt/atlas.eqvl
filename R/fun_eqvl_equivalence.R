# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
# No packages required

# [FUNCTIONS] ------------------------------------------
# - Generalized logistic function -----------------------------------------
fun_eqvl_logistic <- function(
    x,
    a = 0,
    k = 1,
    c = 1,
    q = 1,
    m = 0,
    b = 1,
    nu = 1
){
  
  # Arguments validated in main functions
  
  # Generalized logistic function
  y <- a + (k - a) / ((c + q * exp(-b * (x - m))) ^ (1 / nu))
  
  # output
  return(y)
  
}

# - Equivalence function ------------------------------------
fun_eqvl_equivalence <- function(
    dbl_var
    , dbl_scale_lb = NULL
    , dbl_scale_ub = NULL
    , dbl_scaling = 0.5
){
  
  # Argument validation
  stopifnot(
    "'dbl_var' must be a numeric vector or matrix." =
      is.numeric(dbl_var)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be either NULL or numeric." =
      any(
        is.numeric(dbl_scale_lb)
        , is.null(dbl_scale_lb)
      )
  )
  
  stopifnot(
    "'dbl_scale_ub' must be either NULL or numeric." =
      any(
        is.numeric(dbl_scale_ub)
        , is.null(dbl_scale_ub)
      )
  )
  
  stopifnot(
    "'dbl_scaling' must be a non-negative number." =
      all(
        is.numeric(dbl_scaling)
        , dbl_scaling >= 0
      )
  )
  
  # Normalize data to percentage scale
  if(all(
    length(dbl_scale_lb),
    length(dbl_scale_ub)
  )){
    
    # Normalize by scale bounds
    dbl_var / (
      dbl_scale_ub[[1]] -
        dbl_scale_lb[[1]]
    ) -
      dbl_scale_lb[[1]] / (
        dbl_scale_ub[[1]] -
          dbl_scale_lb[[1]]
      ) -> dbl_var
    
  } else if(!all(
    max(dbl_var, na.rm = T) <= 1,
    min(dbl_var, na.rm = T) >= 0
  )){
    
    # Normalize by maxima
    dbl_var /
      max(
        dbl_var
        , na.rm = T
      ) -> dbl_var
    
  }
  
  dbl_var[all(
    dbl_var <= 1,
    dbl_var >= 0
  )] -> dbl_var
  
  # Define variable and midpoint
  dbl_var -> x
  rm(dbl_var)
  
  dbl_scaling -> m
  rm(dbl_scaling)
  
  # Calculate equivalence
  fun_eqvl_logistic(
    x = x,
    m = m,
    a = 0,
    k = x,
    c = 1,
    q = m * (1 - x),
    nu = x / m,
    b = 1 / (1 - m)
    # b = 1 / (x * (1 - m)) #nope
    
    # b = 1 / (1 - m) ^ x
    # b = 1 / (1 - m) ^ (1 - x)
    # b = 1 / (1 - m) ^ (1 / x)
    # b = 1 / (1 - m) ^ (1 / (1 - x))
    # b = 1 / cos((pi/2) * (1 - x * (1 - m)))
    # b = 1 / cos((pi/2) * (1 - x) ^ (1 - m))
    # b = 1 / cos((pi/2) * x ^ (1 - m))
    # b = tan(acos(1 - m))
    # b = tan(acos(1 - m))
    # b = tan(acos(x * (1 - m)))
    # b = tan((pi/2) * m)
    # b = tan((pi/2) * m * (1 - x))
    # b = tan((pi/2) * m ^ x)
    # b = 30 / (x * (1 - m)) #too specialized
    # b = tan((pi/2) * cos((pi/2) * x * (1 - m))) #too specialized
    # b = tan((pi/2) * (cos((pi/2) * x * (1 - m)) ^ (1 - m))) #too specialized
  ) -> dbl_equivalence
  
  # x ^ ((1 / x) * (1 / (1 - m))) -> dbl_equivalence
  # x ^ ((1 / x) ^ (1 / x)) -> dbl_equivalence
  # x ^ ((1 / x) ^ (m / x)) -> dbl_equivalence
  # x -> dbl_equivalence
  # x ^ 2 -> dbl_equivalence
  # x * (x ^ (1 / (1 - m))) + (1 - x) * x -> dbl_equivalence
  # x * (x ^ 2) + (1 - x) * x -> dbl_equivalence
  # x ^ (1 / (1 - m)) -> dbl_equivalence
  # x ^ (1 / x * (1 - m)) -> dbl_equivalence
  
  # Output
  return(dbl_equivalence)
  
}

# - Attribute equivalence function ----------------------------------------
fun_eqvl_attribute <- function(
    dbl_profile,
    dbl_midpoint,
    dbl_scale_lb = 0
){
  
  # arguments validation
  stopifnot(
    "'dbl_profile' must be a numeric vector of professional attributes greater or equal to 'dbl_scale_lb'." = 
      any(
        is.na(dbl_profile),
        all(
          is.numeric(dbl_profile)
          , dbl_profile >= 
            dbl_scale_lb
        )
      )
  )
  
  stopifnot(
    "'dbl_midpoint' must be a scalar between 0 and 1 of indicating the midpoint for calculating equivalence." = 
      any(
        is.na(dbl_midpoint),
        all(
          is.numeric(dbl_midpoint)
          , dbl_midpoint >= 0
          , dbl_midpoint <= 1
        )
      )
  )
  
  stopifnot(
    "'dbl_scale_lb' must be a scalar of indicating the scale's lower bound." = 
      any(
        is.na(dbl_scale_lb),
        is.numeric(dbl_scale_lb)
      )
  )
  
  # data wrangling
  dbl_midpoint[[1]] -> dbl_midpoint
  
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  # normalize by scale's lb
  # and profile's max attribute
  dbl_profile / (
    max(dbl_profile) -
      dbl_scale_lb
  ) -
    dbl_scale_lb / (
      max(dbl_profile) -
        dbl_scale_lb
    ) -> dbl_profile
    
  rm(dbl_scale_lb)
  
  # define variable and midpoint
  dbl_profile -> x
  rm(dbl_profile)
  
  dbl_midpoint -> m
  rm(dbl_midpoint)
  
  # calculate attribute equivalence
  # with generalized logistic function
  fun_eqvl_logistic(
    x,
    a = 0,
    k = x,
    c = 1,
    q = m * (1 - x),
    m = m,
    b = 1 / (1 - m),
    nu = x / m
  ) -> dbl_attribute_eqvl
  
  rm(x, m)
  
  # output
  return(dbl_attribute_eqvl)
  
}

# - Similarity equivalence function ------------------------------------------------
fun_eqvl_similarity <- function(
    dbl_similarity,
    dbl_midpoint
){
  
  # arguments validation
  stopifnot(
    "'dbl_similarity' must be a numeric vector of similarity scores between 0 and 1." = 
      any(
        is.na(dbl_similarity),
        all(
          is.numeric(dbl_similarity)
          , dbl_similarity >= 0
          , dbl_similarity <= 1
        )
      )
  )
  
  stopifnot(
    "'dbl_midpoint' must be a numeric scaling vector with values between 0 and 1 and the same length as 'dbl_similarity'." = 
      any(
        is.na(dbl_midpoint),
        all(
          is.numeric(dbl_midpoint)
          , dbl_midpoint >= 0
          , dbl_midpoint <= 1
          , length(dbl_similarity) ==
            length(dbl_midpoint)
        )
      )
  )
  
  # define variable and midpoint
  dbl_similarity -> x
  rm(dbl_similarity)
  
  dbl_midpoint -> m
  rm(dbl_midpoint)
  
  # calculate scaled similarity
  # with generalized logistic function
  fun_eqvl_logistic(
    x,
    a = 0,
    k = x,
    c = 1,
    q = m * (1 - x),
    m = m,
    b = 1 / (1 - m),
    # b = factorial(m + 1)?
    nu = x / m
  ) -> dbl_similarity_eqvl
  
  rm(x, m)
  
  # output
  return(dbl_similarity_eqvl)
  
}

# - Tau (education + xp) equivalence function --------------------------------------
fun_eqvl_years <- function(
    dbl_years,
    dbl_years_min,
    dbl_scaling
){
  
  # arguments validation
  stopifnot(
    "'dbl_years' must be a non-negative scalar of years of education and experience." = 
      any(
        is.na(dbl_years),
        all(
          is.numeric(dbl_years),
          dbl_years >= 0
        )
      )
  )
  
  stopifnot(
    "'dbl_years_min' must be a non-negative numeric vector of required years of education and experience." = 
      any(
        is.na(dbl_years_min),
        all(
          is.numeric(dbl_years_min),
          dbl_years_min >= 0
        )
      )
  )
  
  stopifnot(
    "'dbl_scaling' must be a numeric scaling vector with values between 0 and 1 and the same length as 'dbl_years_min'." = 
      any(
        is.na(dbl_scaling),
        all(
          is.numeric(dbl_scaling)
          , dbl_scaling >= 0
          , dbl_scaling <= 1
          , length(dbl_years_min) ==
            length(dbl_scaling)
        )
      )
  )
  
  # data wrangling
  dbl_years[[1]] -> dbl_years
  
  # calculate tau (education + xp) equivalence
  # with generalized logistic function
  fun_eqvl_logistic(
    x = dbl_years,
    a = 0,
    k = 1,
    c = 1,
    q = 1,
    m = dbl_years_min,
    # b = dbl_years_min / (1 - dbl_scaling), #setup mechanism to avoid NaN
    b = dbl_years_min,
    # b = dbl_years_min * dbl_scaling,
    nu = 1
  ) -> dbl_years_eqvl
  
  # Output
  return(dbl_years_eqvl)
  
}

# # [TEST] ------------------------------------------------------------------
# # - Test ------------------------------------------------------------------
# fun_eqvl_equivalence(runif(1, 0, 1))
