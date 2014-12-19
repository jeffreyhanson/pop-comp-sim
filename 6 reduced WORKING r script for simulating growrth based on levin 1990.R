
yfun<-function(r,V,K)
{
	V*r/(K+r)	
}


## here it is for a few days 

## here it is for a few days 

# universal variables
r0=1500 # intial resource amount
E= 0.000005 # amount of resource used per cell per hour
K= 400 # the amount of resources at which the growth rate is half of V
timestep= 1/60  #0.05 # the length of each timestep (in hours)
totaltime= 24 # the total length of time to be simulated

# variables for each strain
V.1= 1.6 # max theoretical growth rate (per hour) for strain 1
V.2= 1.6 # max theoretical growth rate (per hour) for strain 2
d.1= -0.005*V.1 # the death rate for strain 1 per hour (here i just made it relative to the max growth)
d.2= -0.01*V.2 # the death rate for strain 2 per hour (here i just made it relative to the max growth)



# initial pop sizes
N1.0=100000
N2.0=100000
Na.0= N1.0+N2.0

#create object for starting sizes each day 
days=25
ips.1=rep(0,days)
ips.2=rep(0,days)
ips.a=rep(0,days)

# do first day 
ips.1[1]=N1.0
ips.2[1]=N2.0
ips.a[1]=N1.0+N2.0
  
  
  # create the objects to use throughout
  ts=seq(0,totaltime,timestep) # time points to be used
  Ns.1=rep(0,length(ts)) # create object for pop size at each time for strain 1
  ys.1=rep(0,length(ts)) # create object for growth rate at each time for strain 1 
  Ns.2=rep(0,length(ts)) # create object for pop size at each time for strain 2
  ys.2=rep(0,length(ts)) # create object for growth rate at each time for strain 2
  Ns.a=rep(0,length(ts)) # create object for pop size at each time for all strains
  rs=rep(0,length(ts)) # create object for resource amount at each time
  
  ## first step
  
  ys.1[1]= 0 
  ys.2[1]= 0 
  Ns.1[1]= N1.0 
  Ns.2[1]= N2.0 
  Ns.a[1]= Ns.1[1]+Ns.2[1]
  rs[1]= r0  


  for (i in 2:length(ts)){
    
    
    
    ys.1[i]= yfun(rs[i-1],V.1,K) # calculates new growth rate for strain 1
    ys.2[i]= yfun(rs[i-1],V.2,K)  # calculates new growth rate for strain 2
    
    Ns.1[i]= Ns.1[i-1] + ((Ns.1[i-1]*ys.1[i]) * timestep) + (d.1 * Ns.1[i-1] * timestep) # calculates new pop size (after growth and death)
    Ns.2[i]= Ns.2[i-1] + ((Ns.2[i-1]*ys.2[i]) * timestep) + (d.2 * Ns.2[i-1] * timestep) # calculates new pop size (after growth and death)
    Ns.a[i]= Ns.1[i]+Ns.2[i] # calculates new pop size (after growth and death) for all strains
    
 
    rs[i]= rs[i-1] -E*timestep* (Ns.1[i-1]*ys.1[i]*timestep + Ns.2[i-1]*ys.2[i]*timestep) # calculates new resource amount (after growth and death)
    if(rs[i]<0)
    	rs[i]=0
    
  }
  
  
  #check objects
  #head(ts)
  #head(rs)
  #head(Ns.1)
  #head(Ns.2)
  #head(Ns.a)
  #head(ys.1)
  #head(ys.2)
  # plot the objects
  #plot(ts,rs)
  #plot(ts,ys.1)
  #plot(ts,ys.2)
  
  # quartz("first plot",6,6)
  plot(ts,Ns.a, type="n")
  points(ts,Ns.1,type="l", col="blue")
  points(ts,Ns.2,type="l", col="red")
  x11()

##########################################################################
## now loop through the days

for(j in 2:length(ips.a)){

N1.0=Ns.1[length(Ns.1)]/1000 # initial pop size
N2.0=Ns.2[length(Ns.2)]/1000 # initial pop size
Na.0= N1.0+N2.0
ips.1[j]=N1.0
ips.2[j]=N2.0
ips.a[j]=N1.0+N2.0


# create the objects to use throughout
ts=seq(1,totaltime,timestep) # time points to be used
Ns.1=rep(0,length(ts)) # create object for pop size at each time for strain 1
ys.1=rep(0,length(ts)) # create object for growth rate at each time for strain 1 
Ns.2=rep(0,length(ts)) # create object for pop size at each time for strain 2
ys.2=rep(0,length(ts)) # create object for growth rate at each time for strain 2
Ns.a=rep(0,length(ts)) # create object for pop size at each time for all strains
rs=rep(0,length(ts)) # create object for resource amount at each time

## first step

ys.1[1]= V.1*r0/(K+r0) # calculates first growth rate for strain one
ys.2[1]= V.2*r0/(K+r0) # calculates first growth rate for strain two
Ns.1[1]= N1.0 + (N1.0*ys.1[1]*timestep) + (d.1 * N1.0 *timestep) # calculates first pop size (after growth) for strain 1
Ns.2[1]= N2.0 + (N2.0*ys.2[1]*timestep) + (d.2 * N2.0 *timestep) # calculates first pop size (after growth) for strain 2
Ns.a[1]= Ns.1[1]+Ns.2[1]
rs[1]= r0-E*(N1.0*ys.1[1]*timestep+N2.0*ys.2[1]*timestep) # calculates first new resource amount (after growth)

for (i in 2:length(ts)){
  #i=2
  # making death rates kick in later 
  #if (rs[i-1] > E) # if there are still resources left
    #d.1=0
  #if (rs[i-1] > E) # if there are still resources left
    #d.2=0
  #if (rs[i-1] < E) # if there are no resources left
    #d.1=-0.01*V.1
  #if (rs[i-1] < E) # if there are no resources left
    #d.2=-0.02*V.2
    
  if (rs[i-1] > E) # if there are still resources left
    ys.1[i]= V.1*rs[i-1]/(K+rs[i-1]) # calculates new growth rate for strain 1
  if (rs[i-1] > E) # if there are still resources left
    ys.2[i]= V.2*rs[i-1]/(K+rs[i-1]) # calculates new growth rate for strain 2
  if (rs[i-1] < E) # if all the resources are gone, the growth rate is zero
    ys.1[i]= 0 # calculates new growth rate
  if (rs[i-1] < E) # if all the resources are gone, the growth rate is zero
    ys.2[i]= 0 # calculates new growth rate
  Ns.1[i]= Ns.1[i-1] + ((Ns.1[i-1]*ys.1[i]) * timestep) + (d.1 * Ns.1[i-1] * timestep) # calculates new pop size (after growth and death)
  Ns.2[i]= Ns.2[i-1] + ((Ns.2[i-1]*ys.2[i]) * timestep) + (d.2 * Ns.2[i-1] * timestep) # calculates new pop size (after growth and death)
  Ns.a[i]= Ns.1[i]+Ns.2[i] # calculates new pop size (after growth and death) for all strains
  
  # okay so thinsg got complicated when trying to do the change in r for multiple strains
  # so all that it should be is the old r minus E times the number of new bacteria
  # nope thats not true because if the bacteria grow but then died then I should be useing the growth rate times 
  #   the number at the beginning of the interval (scaled by the time steps)
  ## if (rs[i-1] > 0.0005) # if there are still resources left
  
  ##i=100
  rs[i]= rs[i-1] -E*timestep* (Ns.1[i-1]*ys.1[i]*timestep + Ns.2[i-1]*ys.2[i]*timestep) # calculates new resource amount (after growth and death)
  
}
length(ips.a)
points(ts,Ns.a, type="l")
points(ts,Ns.1,type="l", col="blue")
points(ts,Ns.2,type="l", col="red")

}
# quartz("second plot",6,6)
# quartz("second plot",6,6)
length(daysp)
daysp=seq(1,days,1)
plot(ips.a[2:days]~daysp[2:days], type="l",ylim=c(0,max(ips.a)))
points(ips.1[2:days]~daysp[2:days], col= "blue", type="l")
points(ips.2[2:days]~daysp[2:days], col= "red", type="l")