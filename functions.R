calcy=function(r,V,K) {
	V*r/(K+r)	
}

makePlot=function(r0, E, K, timestep, totaltime, N1, V1, d1, N2, V2, d2) {
	#### Initialization
	# create dataframe with results
	expDF=data.frame(	time=seq(0,totaltime,timestep), 
						Ns1=numeric(1), 
						ys1=numeric(1), 
						Ns2=numeric(1),
						ys2=numeric(1),
						Nsa=numeric(1),
						rs=numeric(1)
					)
	# set initial conditions
	expDF[1,]=c(0,N1,0,N2,0,N1+N2,r0)

	#### Main processing
	for (i in 2:nrow(expDF)) {
		expDF$ys1[i]= calcy(expDF$rs[i-1],V1,K) # calculates new growth rate for strain 1
		expDF$ys2[i]= calcy(expDF$rs[i-1],V2,K)  # calculates new growth rate for strain 2
		expDF$Ns1[i]= expDF$Ns1[i-1] + ((expDF$Ns1[i-1]*expDF$ys1[i]) * timestep) + (d1 * expDF$Ns1[i-1] * timestep) # calculates new pop size (after growth and death)
		expDF$Ns2[i]= expDF$Ns2[i-1] + ((expDF$Ns2[i-1]*expDF$ys2[i]) * timestep) + (d2 * expDF$Ns2[i-1] * timestep) # calculates new pop size (after growth and death)
		expDF$rs[i]= expDF$rs[i-1] -E*timestep* (expDF$Ns1[i-1]*expDF$ys1[i]*timestep + expDF$Ns2[i-1]*expDF$ys2[i]*timestep) # calculates new resource amount (after growth and death)
		if(expDF$rs[i]<0)
			expDF$rs[i]=0
	}
	expDF$Nsa = expDF$Ns1 + expDF$Ns2
	
	### Exports
	plot(0, type="n", ylab="Population size", xlab="Time (hours)", xlim=c(0, totaltime), ylim=c(0, max(c(expDF$Nsa, expDF$Ns1, expDF$Ns2))))
	lines(expDF$time, expDF$Ns1, col=cols[1])
	lines(expDF$time, expDF$Ns2, col=cols[2])
	lines(expDF$time, expDF$Nsa, col=cols[3])
	legend("topleft", legend=c("Strain 1", "Strain 2", "Total"), lty=rep(1,3), col=cols[1:3])
}
