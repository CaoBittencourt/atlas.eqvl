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
    , dbl_scaling = 1
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
    b = tan((pi/2) * cos((pi/2) * x * (1 - m))),
    # b = tan((pi/2) * (cos((pi/2) * x * (1 - m)) ^ (1 - m))),
    nu = x / m
  ) -> dbl_equivalence
  
  # Output
  return(dbl_equivalence)
  
}

# - Educational equivalence function --------------------------------------
fun_eqvl_equivalence_years <- function(
    dbl_years,
    dbl_years_min,
    dbl_scaling = 0
){
  
  # Arguments validation
  stopifnot(
    "'dbl_years' must be a non-negative number." =
      all(
        is.numeric(dbl_years),
        dbl_years >= 0
      )
  )
  
  stopifnot(
    "'dbl_years_min' must be a non-negative number." =
      all(
        is.numeric(dbl_years_min),
        dbl_years_min >= 0
      )
  )
  
  stopifnot(
    "'dbl_scaling' must be a number between 0 and 1." =
      all(
        is.numeric(dbl_scaling),
        dbl_scaling >= 0,
        dbl_scaling <= 1
      )
  )
  
  # Data wrangling
  dbl_years[[1]] -> dbl_years
  
  dbl_years_min[[1]] -> dbl_years_min
  
  dbl_scaling[[1]] -> dbl_scaling
  
  # Apply logistic equivalence to years of education
  fun_eqvl_logistic(
    x = dbl_years,
    a = 0,
    k = 1,
    c = 1,
    q = 1,
    m = dbl_years_min,
    b = dbl_years_min / (1 - dbl_scaling),
    nu = 1
  ) -> dbl_equivalence_edu
  
  # Output
  return(dbl_equivalence_edu)
  
}

# # [TEST] ------------------------------------------------------------------
# # - Test ------------------------------------------------------------------
# fun_eqvl_equivalence(runif(1, 0, 1))
