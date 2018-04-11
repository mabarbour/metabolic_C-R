
C_to_K <- function(C) C + 273.15 # convert degrees C to Kelvin for Arrhenius equation

arrhenius_eqn <- function(E_a,   # activation energy of metabolic processes, units = eV/K
                          T)     # temperature in Kelvin
{
  euler <- exp(1)  # Euler's number
  k <- 8.6173303 * 10^(-5) # Boltzmann constant, units = eV (electron volt)
  
  euler^(-E_a/(k*T))
}

arrhenius_eqn(E_a = 0.65, T = C_to_K(20))
