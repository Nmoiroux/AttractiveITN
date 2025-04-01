

### modify fRTP function to be fed with all parameters of the IC function ----
fRTP_sens <- function(nsim, S = 0.9, g = 3, Nh = 1000, Ih = 0.5, k = 0.1, n = 11, m1u = 0.05,
											m1p = 0.72, 
											m2u = 0.005,
											m2p = 0.21,
											Du = 0.43,
											Dp = 0.3,
											Uh = 0.6,
											pi = 0.9, 
											#Pllin = 0.5,  
											FUN = IC){
	RTP <- FUN(nsim,S = S, g = g, Nh = Nh, Ih = Ih, k = k, n = n, 
						 m1u=m1u,
						 m1p=m1p, 
						 m2u=m2u,
						 m2p=m2p,
						 Du=Du,
						 Dp=Dp,
						 Uh=Uh,
						 pi=pi, 
		 				 Pllin=pref_ref)["IC"] #/ +
		# FUN(nsim,S = S, g = g, Nh = Nh, Ih = Ih, k = k, n = n, 
		# 		m1u=m1u,
		# 		m1p=m1p, 
		# 		m2u=m2u,
		# 		m2p=m2p,
		# 		Du=Du,
		# 		Dp=Dp,
		# 		Uh=Uh,
		# 		pi=pi, 
		# 		Pllin=pref_ref)["IC"]
	return(RTP)
}
