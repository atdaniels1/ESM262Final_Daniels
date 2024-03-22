#'Land Footprint of Wind Turbines
#'
#'This function calculates the land footprint of a wind turbine based on turbine configuration,
#'rotor diameter, and rated capacity of the turbine.
#'@param rd Rotor Diameter (m)
#'@param pwr Rated capacity of the turbine (MW)
#'@param nt Total number of turbines default = 25
#'@param CF Capacity Factor of the wind turbine default = 0.2
#'@param Y Lifetime of the plant (years) default = 20
#'@return L Land Footprint of Turbine (m2/GWh)
#'Function Definition

land_footprint = function(rd,pwr,nt=25, CF=0.2, Y=20)
{

  rd = ifelse(rd<0, stop("rotor diameter cannot be negative"), rd)
  pwr = ifelse(pwr<0, stop("rated power cannot be negative"), pwr)
  tpd = pwr/(nt*(rd^2))
  wfd= tpd * nt
  lf = 1000/(wfd*CF*8760*Y)
  return(lf)

}
