calcy=function(r,V,K) {
	V*r/(K+r)	
}

plot1=function(r0, E, timestep, totaltime, N1, V1, d1, N2, V2, d2) {
	#### Initialization
	# create dataframe with results
	expDF=data.frame(	time=seq(0,totaltime,timestep), 
						Ns1=numeric(0), 
						ys1=numeric(0), 
						Ns2=numeric(0),
						ys2=numeric(0),
						Nsa=numeric(0),
						rs=numeric(0)
					)
	# set initial conditions
	expDF[,1]=c(0,0,0,N1,0,N2,0,N1+N2,r0)

	#### Main processing
	for (i in 2:nrow(expDF)) {
		expDF$ys1[i]= calcy(rs[i-1],V1,K) # calculates new growth rate for strain 1
		expDF$ys2[i]= calcy(rs[i-1],V2,K)  # calculates new growth rate for strain 2
		
		expDF$Ns1[i]= expDF$Ns1[i-1] + ((expDF$Ns1[i-1]*expDF$ys1[i]) * timestep) + (expDF$d1 * expDF$Ns1[i-1] * timestep) # calculates new pop size (after growth and death)
		expDF$Ns2[i]= expDF$Ns2[i-1] + ((expDF$Ns2[i-1]*expDF$ys2[i]) * timestep) + (expDF$d2 * expDF$Ns2[i-1] * timestep) # calculates new pop size (after growth and death)

		expDF$rs[i]= expDF$rs[i-1] -E*timestep* (expDF$Ns1[i-1]*expDF$ys1[i]*timestep + expDF$Ns2[i-1]*expDF$ys2[i]*timestep) # calculates new resource amount (after growth and death)
		if(rs[i]<0)
			rs[i]=0
	}
	expDF$Nsa = expDF$Ns1 + expDF$Ns2
	
	### Exports
	plot(0, ylab="Population size", xlab="Time (hours)", xlim=c(0, totaltime), ylim=c(0, max(c(expDF$Nsa, expDF$Ns1, expDF$Ns2))))
	points(expDF$time, expDF$Ns1, pch=16, col="black", bg="blue", lwd=3)
	points(expDF$time, expDF$Ns2, pch=16, col="black", bg="green", lwd=3)
	points(expDF$time, expDF$Nsa, pch=16, col="black", bg="red", lwd=3)
	legend("topright", legend=c("Strain 1", "Strain 2", "Total"), pch=rep(16,3), col=rep("black", 3), pt.bg=c("blue", "green", "red"), pt.lwd=3)
}

plot2=function(r0, E, timestep, totaltime, N1, V1, d1, N2, V2, d2) {
	#### Initialization
	# create dataframe with results
	tempDF=data.frame(	time=seq(0,totaltime,timestep), 
						Ns1=numeric(0), 
						ys1=numeric(0),
						Ns2=numeric(0),
						ys2=numeric(0),
						Nsa=numeric(0),
						rs=numeric(0)
					)
	expDF=data.frame(time=seq(0, totaltime, 1),
					  ips1=numeric(0),
					  ips2=numeric(0),
					  ipsa=numeric(0),
	)
					
	# set initial conditions
	tempDF[,1]=c(0,0,0,N1,0,N2,0,N1+N2,r0)
	expDF2[,1]=c(0,0,0,0)

	#### Main processing
	for (i in 2:nrow(expDF2)) {
	
		for (j in 2:nrow(tempDF)) {
			if (tempDF$rs[i-1] > E) # if there are still resources left
				tempDF$ys1[i] = V1*tempDF$rs[i-1]/(K+tempDF$rs[i-1]) # calculates new growth rate for strain 1
			if (rs[i-1] > E) # if there are still resources left
				tempDF$ys2[i] = V2*tempDF$rs[i-1]/(K+tempDF$rs[i-1]) # calculates new growth rate for strain 2
			if (rs[i-1] < E) # if all the resources are gone, the growth rate is zero
				tempDF$ys1[i] = 0 # calculates new growth rate
			if (rs[i-1] < E) # if all the resources are gone, the growth rate is zero
				tempDF$ys2[i] =0 # calculates new growth rate
			
			tempDF$Ns1[i]= tempDF$Ns1[i-1] + ((tempDF$Ns1[i-1]*tempDF$ys1[i]) * timestep) + (tempDF$d1 * tempDF$Ns1[i-1] * timestep) # calculates new pop size (after growth and death)
			tempDF$Ns2[i]= tempDF$Ns2[i-1] + ((tempDF$Ns2[i-1]*tempDF$ys2[i]) * timestep) + (tempDF$d2 * tempDF$Ns2[i-1] * timestep) # calculates new pop size (after growth and death)
			tempDF$rs[i]= tempDF$rs[i-1] -E*timestep* (tempDF$Ns1[i-1]*tempDF$ys1[i]*timestep + tempDF$Ns2[i-1]*tempDF$ys2[i]*timestep) # calculates new resource amount (after growth and death)
		}
		expDF$Nsa = expDF$Ns1 + expDF$Ns2
		
	
	}
	#### Exports
	
	points(ts,Ns.a, type="l")
points(ts,Ns.1,type="l", col="blue")
points(ts,Ns.2,type="l", col="red")





}




