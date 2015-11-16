import time
import random
import math
import pandas as pd
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import numpy as np
# to do : 1. add co2 calc to cost function
#            A. calculate route co2
#               1. add installation mass estimate - done
#               2. add recycling energy estimate - done
#            B. calculate pre-route co2
#         2. trips calc for transportation - done
#            A. add trips variable - done
#            B. trips as a multiple interger for capacity- done
#         3. visualize results
#            A. map: red points start, green pts end
#            B. pie chart share of facilities co2
#            C. bar chart share of facilities mass
#         4. add item, destination and capacity automatically - done

# constants
carbon_intensity=.07315 #mtco2 per MMBTU
hv=0.128450 #MMBTU per gal diesel
mpg=11 #miles per gal
cap=1.10231e8 #truck capacity in metric tons (50,000 lbs)
km_per_mile=1.609 #km per mile
recy=0.005 # recycling energy MWh/kg recovered
recov=0.2 # percent recovered
ton_to_lbs=2000
MWh_to_kWh=1000

efactors = {}
with open('emissionfactors/emissionfactors_lbskwh.csv') as f:
    next(f)
    for line in f:
        state,abbrev,co2,ch4,n20,so2,nox,hg=line.strip().split(',')[0:8]
        if abbrev != '':
            efactors.setdefault((abbrev),[])
            efactors[(abbrev)].append((float(co2),float(ch4),float(n20))) #lbs/kwh
f.close()
#print(efactors)

routes={}
# 
with open('codeR1.txt') as f:# reading routes for optimization purposes df
    next(f) # skip header
    for line in f:
        index,origin,dest,land,r_dist,cl_dist,size,state=line.strip().split(',')
        if r_dist != 'NA':
            routes.setdefault((origin,dest),[])
            # Add details to the list of possible routes
            routes[(origin,dest)].append((land,float(r_dist),float(cl_dist),float(size),state))
f.close()
#print(routes)

item=list()
routekeys=list(routes.keys())
for d in routekeys: 
    item.append((d[0], routes[d][1][2]))
print("item is:",item)

destination = list(routes.keys())[1][1]

capacity = {} # facility capacity
for d in routes[routekeys[1]]:
    capacity[d[0]]=100e4
#print("capacity is:",capacity)

locations = {}

with open('scheduleR1.csv') as f:# reading codes for mapping purposes
    next(f)
    for line in f:
        i,f_lon,f_lat,to_lon,to_lat,r_distance,cl_distance,size,nos,to_state=line.strip().split(',')
        locations.setdefault((f_lon,f_lat),[])
        locations[(f_lon,f_lat)].append((float(to_lon.replace('"','')),
                                         float(to_lat.replace('"','')),
                                         float(size.replace('"','')),
                                         int(nos.replace('"','')),
                                         to_state))
f.close()
#print(locations)
    

def getminutes(t):
    x=time.strptime(t,'%H:%M')
    return x[3]*60+x[4]

def printschedule(r):
    # input schedule for outbound route of each item
    for d in range(int(len(r))):
        origin=item[d][0]
        out=routes[(origin,destination)][int(r[d])]
        print('%10s%5s %5s %3skm %5sMW %3s' % (origin,destination,
                                                  out[0],int(out[1]), int(out[3]/1000), out[4]))
                                                  
def plotschedule(r):
    lats = []
    lons = []
    #input schedule for outbound route of each item 
    for d in range(int(len(r))):
        lats.append(locations[(f_lon,f_lat)][int(r[d])][0])
        lons.append(locations[(f_lon,f_lat)][int(r[d])][1])
        print(lats,lons) 
    # Create a map, using the Gall–Peters projection, 
    map = Basemap(projection='gall',
          # with low resolution,
          resolution = 'h',
          # And threshold 100000
          area_thresh = 10.0,
          # Centered at US (i.e null island)
          lat_0=38.901014, lon_0=-77.031317)
    map.drawcoastlines()
    map.drawcountries()
    map.fillcontinents(color = 'coral')
    map.drawmapboundary()
    map.plot(lats,lons, 'ro', markersize=12)
    plt.show()
        
def schedulecost(sol):
    totalprice=0
    capfee=0
    cenfee=0
    elec=0
    weight = {}
    tot={}
    for i in capacity:
        weight.setdefault((i),[])
        tot.setdefault((i),[])
    for d in range(int(len(sol))):
        # sol is the solution set, a list
        # Get the outbound routes
        origin=item[d][0]
        outbound=routes[(origin,destination)][int(sol[d])]
        # get the size
        size = item[d][1]
        ## convert size to mass
        mass=.09411*size #  8 kg/85W * kW in mt
        #print("mass is:",mass)
        # get trips
        trips=int(mass/cap)
        ## extract the destination id
        ent=outbound[0]
        # recycling energy depends on state of facility
        state=outbound[4]
        #print('state is',state)
        #print('efactors are',efactors[(state)][0][0])#,efactors[(state)][0][1],efactors[(state)][0][2])
        co2=efactors[(state)][0][0]*mass*recov*recy*MWh_to_kWh*1000/(ton_to_lbs)# tons co2  
        ch4=efactors[(state)][0][1]*mass*recov*recy*MWh_to_kWh*1000/(ton_to_lbs)
        no2=efactors[(state)][0][2]*mass*recov*recy*MWh_to_kWh*1000/(ton_to_lbs)
        elec+=co2+ch4*25+no2*298 # GWP
        ## assign mass to facility
        weight[ent].append(int(mass))
   
       
        # Total price is the co2 kg of all outbound routes
        totalprice+=(carbon_intensity*hv*(trips*outbound[1]+outbound[2]))/(km_per_mile*mpg)#mt
        #totalprice+=returnf[2]

    for i in weight: 
        k =0
        for j in weight[i][:]:
            k+=j
        tot[i].append(int(k))
    # check against capacity
    for i in capacity:
        #print(tot[i][0],capacity[i])
        #print(type(capacity[i]),type(tot[i][0]))
        if int(tot[i][0])>int(capacity[i]): 
            # penalize for overcapacity
            capfee+=0
        if int(tot[i][0])>0:
            # penalize for decentralization
            cenfee+=0
    #print('electricity impact is',int(elec))        
    return int(totalprice+capfee+cenfee+elec)

def randomoptimize(domain,costf):
    # costf refers to the cost function
    best=999999999
    bestr=None
    for i in range(1,1000):
        # Create a random solution
        r=[float(random.randint(domain[i][0],domain[i][1]))
        for i in range(len(domain))]
        # Get the cost
        cost=costf(r)
        
        # Compare it to the best one so far
        if cost<best:
            best=cost
            bestr=r 
            #print(best)    
    return bestr

def hillclimb(domain,costf):
    # Create a random solution
    sol=[random.randint(domain[i][0],domain[i][1])
      for i in range(len(domain))]
    # Main loop
    while 1:
    # Create list of neighboring solutions
        neighbors=[]
    
        for j in range(len(domain)):
            # One away in each direction
            
            if sol[j]==domain[j][0]:
                neighbors.append(sol[0:j]+[sol[j]+1]+sol[j+1:])
            if sol[j]==domain[j][1]:
                neighbors.append(sol[0:j]+[sol[j]-1]+sol[j+1:])
        print(sol)
        print(neighbors)
        # See what the best solution amongst the neighbors is
        current=costf(sol)
        best=current
        for j in range(len(neighbors)):
            cost=costf(neighbors[j])
            if cost<best:
                best=cost
                sol=neighbors[j]
            

    # If there's no improvement, then we've reached the top
        if best==current:
            break
    return sol

def annealingoptimize(domain,costf,T=10000.0,cool=0.95,step=1):
    # Initialize the values randomly
    vec=[float(random.randint(domain[i][0],domain[i][1])) 
       for i in range(len(domain))]
  
    while T>0.1:
        # Choose one of the indices
        i=random.randint(0,len(domain)-1)

        # Choose a direction to change it
        dir=random.randint(-step,step)

        # Create a new list with one of the values changed
        vecb=vec[:]
        vecb[i]+=dir
        if vecb[i]<domain[i][0]: vecb[i]=domain[i][0]
        elif vecb[i]>domain[i][1]: vecb[i]=domain[i][1]

        # Calculate the current cost and the new cost
        ea=costf(vec)
        eb=costf(vecb)
        p=pow(math.e,(-eb-ea)/T)

        # Is it better, or does it make the probability
        # cutoff?
        if (eb<ea or random.random()<p):
            vec=vecb      

        # Decrease the temperature
        T=T*cool
    return vec

def geneticoptimize(domain,costf,popsize=50,step=1,
                    mutprob=0.2,elite=0.2,maxiter=100):
    # Mutation Operation
    def mutate(vec):
        i=random.randint(0,len(domain)-1)
        if random.random()<0.5 and vec[i]>domain[i][0]:
            return vec[0:i]+[vec[i]-step]+vec[i+1:] 
        elif vec[i]<domain[i][1]:
            return vec[0:i]+[vec[i]+step]+vec[i+1:]
  
    # Crossover Operation
    def crossover(r1,r2):
        i=random.randint(1,len(domain)-2)
        return r1[0:i]+r2[i:]

    # Build the initial population
    pop=[]
    for i in range(popsize):
        vec=[random.randint(domain[i][0],domain[i][1]) 
             for i in range(len(domain))]
        pop.append(vec)
  
    # How many winners from each generation?
    topelite=int(elite*popsize)
    print('topelite is:'+ str(topelite))
    # Main loop 
    for i in range(maxiter):
        scores=[(costf(v),v) for v in pop]
        scores.sort()
        ranked=[v for (s,v) in scores]
    
    # Start with the pure winners
        pop=ranked[0:topelite]
    
    # Add mutated and bred forms of the winners
    while len(pop)<popsize:
        if random.random()<mutprob:

            # Mutation
            c=random.randint(0,topelite-1)
            pop.append(mutate(ranked[c]))
        else:
      
            # Crossover
            c1=random.randint(0,topelite-1)
            c2=random.randint(0,topelite-1)
            print('c1 is: '+str(c1)+' c2 is:' + str(c2))
            print(ranked[c1])
            print(ranked[c2])
            pop.append(crossover(ranked[c1],ranked[c2]))
    
    # Print current best score
        print('scores:'+ str(scores[0][0]))
    
    return scores[0][1]

