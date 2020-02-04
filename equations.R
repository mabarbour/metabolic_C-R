
C_to_K <- function(C) C + 273.15 # convert degrees C to Kelvin for Arrhenius equation

arrhenius_eqn <- function(E_a,   # activation energy of metabolic processes, units = eV/K
                          T)     # temperature in Kelvin
{
  euler <- exp(1)  # Euler's number
  k <- 8.6173303 * 10^(-5) # Boltzmann constant, units = eV (electron volt)
  
  euler^(-E_a/(k*T))
}

arrhenius_eqn(E_a = 0.65, T = C_to_K(20))

r0 <- function(r_base, # baseline r for a given temperature 
               E_a, 
               T){
  # r_base = r0 * arrhenius_eqn to get observed r value, so:
  r_base/arrhenius_eqn(E_a = E_a, T = T) # = r0
}

r_scaling <- function(r0, E_a, T){
  r0*arrhenius_eqn(E_a = E_a, T = T)
}

# example of r scaling
Temp_seq = 15:30
r_seq <- r_scaling(r0 = r0(r_base = 2, E_a = 0.65, T = C_to_K(20)), # initialize r at 20 Celsius
                   E_a = 0.65,
                   T = C_to_K(Temp_seq))
plot(r_seq ~ Temp_seq, type = "l")