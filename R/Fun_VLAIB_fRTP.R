#' `TP` return the transmission potential (average number of infectious bites that a vector gives according to a behavior and mortality model)
#'
#' @param nsim unused but required argument, for compatibility / comparison with other functions
#' @param S baseline survival rate
#' @param g duration of gonotrophic cycle (positive integer)
#' @param Du Diversion probability when entering a hut without LLIN 
#' @param Dp Diversion probability when entering a hut with LLIN 
#' @param m1u pre-bite feeding related mortality probability when faced to un unprotected host 
#' @param m1p pre-bite feeding related mortality probability when faced to un LLIN protected host 
#' @param m2u post-bite mortality probability when feeding on an unprotected host 
#' @param m2p post-bite mortality probability when feeding on an LLIN protected host 
#' @param Nh Number of humans in the community 
#' @param Uh proportion of humans that use LLINs 
#' @param pi proportion of exposure to bite that occurs during which LLIN is in use 
#' @param Pllin preference for LLIN protected human (against unprotected human) as recorded in a dual choice olfactometer (default = 0.5, i.e inert LLIN)
#' @param k infectiousness: probability that a vector become infected (exposed) while taking a blood meal on an infectious host 
#' @param n duration of extrinsic incubation period of the parasite (in days, positive integer)
#' @param Ih Plasmodium falciparum prevalence rate in the human population
#'
#' @return a vector of several output elements including the TP. Each element of the returning vector is named:
#' "Eu", "Ep", "Pd", "Sd", "Pf_u", "Pf_p", "Pf", "F_u", "Sf", "Sl", "PfA", "BA","OvA", "Pl", "TP"  
#' @export
#'
#' @examples
#' TP()
#' Tp(Ih = 0.1)
#' TP()["TP"]
TP <- function(nsim=1000, S = S_ref, g = g_ref, Du = Du_ref, Dp= Dp_ref,m1u = m1u_ref, m1p = m1p_ref, m2u = m2u_ref, m2p = m2p_ref, 
									Nh = Nh_ref, Uh = Uh_ref, pi = pi_ref, Pllin = 0.5, k = k_ref, n = n_ref, Ih = Ih_ref){
  
  ## proba of encountering an unprotected or LLIN protected human
  Np <- Uh*pi*Nh                       # average number of people protected by an LLIN - Expression (1)
  Nu <- Nh - Np                        # average number of unprotected people - Expression (2)
  Cpp <- dhyper(2, round(Np), round(Nu), 2, log = FALSE) # probability that a host-seeking Anopheles will be faced to a choice between two LLIN protected hosts - Expression (4)
  Cpu <- dhyper(1, round(Np), round(Nu), 2, log = FALSE) # ... choice between a LLIN protected host and an unprotected host - Expression (5)
  Cuu <- dhyper(0, round(Np), round(Nu), 2, log = FALSE) # ... choice between two unprotected hosts- Expression (6)
  Eu <- Cuu + Cpu * (1 - Pllin)				  # proba to enter a house with an unprotected human - Expression (7)
  Ep <- Cpp + Cpu * Pllin			          # proba to enter a house with a protected human - Expression (8)
  
  
  ## diversion, feeding and mortality probabilities
	fi1_u <- 1 - m1u 		      # Successful feeding probability when attempting to feed (non diverted mosquitoes) in a hut without LLIN - Expression (9)
  fi1_p <- 1 - m1p          # Successful feeding probability when attempting to feed (non diverted mosquitoes) in a hut with LLIN- Expression (12)
  fi_u  <- (1-Du) * fi1_u   # Successful feeding probability when entering a hut with an unprotected human - Expression (10)
  fi_p  <- (1-Dp) * fi1_p   # Successful feeding probability when entering a hut with an LLIN protected human- Expression (13)
  S2u <- 1 - m2u            # post-bite survival in hut with unprotected people - Expression (11)
  S2p <- 1 - m2p            # post-bite survival in hut with LLIN protected people - Expression (14)
  
  
  ### Transition probability from HS (host-seeking) to F (fed) state and from F to HS
  Pd <- Du*Eu + Dp*Ep 			              # Proba that an HS vector will be diverted (to HS next night if it survive) - Expression (15)
  Sd <- S 	  									          # Proba that diverted vector survives to state HS next night - Expression (16)
  Pf_u <- Eu * fi_u								        # Proba that an HS vector will bite successfully (same night) on an unprotected human - Expression (17)
  Pf_p <- Ep * fi_p			                  # Proba that an HS vector will bite successfully (same night) on a LLIN protected human - Expression (18)
  Pf <- Pf_u + Pf_p										    # Proba that an HS vector will bite successfully (same night) on human - Expression (19)
  F_u <- Pf_u / (Pf_u + Pf_p)							# Proportion fed on unprotected humans - Expression (20)
  Sf <- (S2u * F_u + (1-F_u)*S2p) * S^g		# Proba survives from successful bite to HS at next G cycle - Expression (21)
  
  
  ### model average lifetime infectious bites
  
  PfA <- Pf/(1-Pd*Sd)									  # Average proba that a HS vector will survive to take a feed - Expression (22)
  BA <- PfA/(1-Sf*PfA)							  	# Average number of bites which a HS vector will survive to give - Expression (23)
  c <- k * Ih          		              # probability that a vector become infected while taking a blood meal -  Expression (24)
  Pl <- PfA*c/(1-PfA*Sf*(1-c))					# Probability that a vector will acquire Pf during its lifetime - Expression (25)
  
  # Expression (35)

  move <- function(i, g, n){				# this function return 1 if the duration of the combination equals the duration to become infectious (n)
    if ((ceiling((n-g-i)/g)*g+i)==(n-g)){
      return (1)
    } else {
      return (0)
    }
  }
  
  Term1 <- NULL
  Term2 <- NULL
  Term3 <- NULL

  for (i in 0:(n-g)){
    Ng <- ceiling((n-g-i)/g)      # number of gonotrophic cycles required to complete sporogony (giving i diversion events)
    Ng2 <- ceiling((n-2*g-i)/g)   # number of gonotrophic cycles required to complete sporogony (giving i diversion events and the combination finshes by a g)
    Term1[i+1] <- ((Pf*Sf)^Ng) * ((Pd*Sd)^i)    # proba to survive the combination of i and g
    Term2[i+1] <- i+Ng2+move(i,g,n)     	      # n in the binomial coefficient (total number of element)
    Term3[i+1] <- factorial(Term2[i+1]) / (factorial(Term2[i+1]-i)*factorial(i))	# Binomial coefficient : nb of possible order of each combination (with 0 to n-g diversions events)
  }
  
  Term4 <- Term1*Term3	       # Term3 gives the number of possible orders for each combination of (i) diversions and ((n-g-i)/g) feeds
  Sl <- Sf * sum(Term4)				 # Proba that a newly infected vector will survive to HS state as an infectious vector - Expression (26)
  
  TP = Pl*Sl*BA						 # Vector average lifetime infectious bites (= individual Vector capacity) - Expression (27)
  
  ### number of oviposition events - can be used as a fitness indicators in a genetic/evolution model - Expression (30)
  OvA <- (1/(1-(PfA*Sf)))-1    # Average number of oviposition which a HS vector will survive to give (geometric series of first term 1 and reason Sf*Pfa, decreased by 1)
  
  results <- c(Eu, Ep, Pd, Sd, Pf_u, Pf_p, Pf, F_u, Sf, Sl, PfA, BA, OvA, Pl, TP)
  names(results) <- c("Eu", "Ep", "Pd", "Sd", "Pf_u", "Pf_p", "Pf", "F_u", "Sf", "Sl", "PfA", "BA","OvA", "Pl", "TP")
  return(results)
}


# function to calculate RTP (Relative transmission potential)
#' `fRTP` return the relative transmission potential of a scenario against another taken as baseline. It is the ratio of TP
#' between two scenarii. It can use various model that are given as `FUN` argument
#'
#' @param nsim number of simulation (used when FUN is model that used simulation)
#' @param m m1p: pre-bite feeding related mortality probability when faced to un LLIN protected host
#' @param m2 m1p at baseline
#' @param p Pllin: preference for LLIN protected humans (against unprotected humans) as recorded in a dual choice olfactometer (default = 0.5, i.e inert LLIN)
#' @param p2 Pllin at baseline 
#' @param D Diversion probability when entering a hut with LLIN 
#' @param D2 D2 at baseline
#' @param Uh LLIN use rate in the population 
#' @param Uh2 Uh at baseline 
#' @param pi pi: proportion of exposure to bite that occurs during which LLIN is in use 
#' @param pi2 pi at baseline 
#' @param FUN the function used to calculate TP in both scenarios, the average number of infectious bites that a vector gives according to a behavior and mortality model (default=TP)
#'
#' @return the value of Relative transmission potential (RTP)
#' @export
#'
#' @examples
#' fRTP(Uh=0.8, Uh2=0.5)
#' 
fRTP <- function(nsim = 1000,m = m1p_ref, m2= m1p_ref, p = 0.5, p2 = 0.5, D = Dp_ref, D2= Dp_ref, Uh = Uh_ref, Uh2 = Uh_ref, pi = pi_ref, pi2 = pi_ref, FUN = TP){
  RTP <- FUN(nsim, m1p = m , Pllin = p , Dp = D , Uh = Uh , pi = pi)["TP"] / 
         FUN(nsim, m1p = m2, Pllin = p2, Dp = D2, Uh = Uh2, pi = pi2)["TP"]
  names(RTP) <- "RTP"
  return(RTP)
}


#' Function to Calculate RTP (Relative Transmission Potential) in the framework of a sensitivity analysis
#' It modify `fRTP` function to be fed with all parameters of the `TP` function
#'
#' `fRTP_sens` calculates the transmission potential (RTP) of a scenario against a baseline. 
#' The function computes the percentage change in RTP using a specified transmission potential model (`FUN`).
#'
#' @param nsim Integer. Number of simulations (used when `FUN` is a model requiring simulation).
#' @param S baseline survival rate
#' @param g duration of gonotrophic cycle (positive integer)
#' @param Nh Number of humans in the community 
#' @param Ih Plasmodium falciparum prevalence rate in the human population
#' @param k infectiousness: probability that a vector become infected (exposed) while taking a blood meal on an infectious host 
#' @param n duration of extrinsic incubation period of the parasite (in days, positive integer)
#' @param m1u pre-bite feeding related mortality probability when faced to un unprotected host 
#' @param m1p pre-bite feeding related mortality probability when faced to un LLIN protected host 
#' @param m2u post-bite mortality probability when feeding on an unprotected host 
#' @param m2p post-bite mortality probability when feeding on an LLIN protected host 
#' @param Du Diversion probability when entering a hut without LLIN 
#' @param Dp Diversion probability when entering a hut with LLIN 
#' @param Uh proportion of humans that use LLINs 
#' @param pi proportion of exposure to bite that occurs during which LLIN is in use 
#' @param FUN Function. A model function to calculate transmission potential (TP), 
#'   typically the average number of infectious bites a vector gives under specified behavior and mortality parameters.
#'
#' @return Numeric. The RTP expressed as a percentage change.
#' @export
fRTP_sens <- function(nsim, S, g, Nh, Ih, k, n, m1u, m1p, m2u, m2p,	Du,	Dp,	Uh,	pi, 
											pref, pref_ref,  
											FUN = TP){
	RTP <- FUN(nsim,S = S, g = g, Nh = Nh, Ih = Ih, k = k, n = n, 
						 m1u=m1u,
						 m1p=m1p, 
						 m2u=m2u,
						 m2p=m2p,
						 Du=Du,
						 Dp=Dp,
						 Uh=Uh,
						 pi=pi, 
						 Pllin=pref)["TP"] / +
		FUN(nsim,S = S, g = g, Nh = Nh, Ih = Ih, k = k, n = n, 
				m1u=m1u,
				m1p=m1p, 
				m2u=m2u,
				m2p=m2p,
				Du=Du,
				Dp=Dp,
				Uh=Uh,
				pi=pi, 
				Pllin=pref_ref)["TP"]
	return(-(1-RTP)*100)
}
