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
#               2. add recycling energy estimate
#            B. calculate pre-route co2
#         2. trips calc for transportation
#            A. add trips variable
#            B. trips as a multiple interger for capacity
#         3. visualize results
#            A. map: red points start, green pts end
#            B. pie chart share of facilities co2
#            C. bar chart share of facilities mass
#         4. add item, destination and capacity automatically - done
# item = (name,origin, size)
 
# price = distance

#item = [("AAJ", 10859.348 ),
#        ("AAI",23278.949662),
#        ("AAH" ,8692.9),
#        ("AAG", 20906.12),
#        ("AAF" ,4604.964),
#        ("AAE" ,18203.811),
#        ("AAD" ,17360.872),
#        ("AAC" ,31590.422),
#        ("AAB",39549.43),
#        ("AAA",4154.589)]
#capacity = {('1'):[150e4],
#          ('2'):[300e4],
#           ('3'):[150e4],
#          ('4'):[300e4],
#           ('5'):[15e4]}

#destination='RG4'# region
# constants
carbon_intensity=73.15 #kgco2 per MMBTU
hv=0.128450 #MMBTU per gal diesel
mpg_ton=55 #miles-tons per gal
cap=8.39 #truck capacity in metric tons (18500 lbs)
km_per_mile=1.609 #km per mile

routes={}
# 
for line in open('schedule3R2.txt'):# reading routes for optimization purposes df
    index,origin,dest,land,r_dist,cl_dist,size,installs=line.strip().split(',')
    routes.setdefault((origin,dest),[])
    # Add details to the list of possible routes
    routes[(origin,dest)].append((land,float(r_dist),float(size),float(cl_dist),int(installs)))
    
    
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
print("capacity is:",capacity)

locations = {}

for line in open('schedule2R2.txt'):# reading codes for mapping purposes
    nos,f_lon,f_lat,to_lon,to_lat,r_distance,cl_distance,size,nos,to_state=line.strip().split(',')
    locations.setdefault((f_lon,f_lat),[])
    # Add details to the list of possible routes
    locations[(f_lon,f_lat)].append((float(to_lon),float(to_lat),float(r_distance),float(size),float(cl_distance),int(nos)))
#print(locations)
    
#domain= {}
#domain=[(0,len(capacity)-1)]*len((item)*1) 

def getminutes(t):
    x=time.strptime(t,'%H:%M')
    return x[3]*60+x[4]

def printschedule(r):
    # input schedule for outbound route of each item
    for d in range(int(len(r))):
        name=item[d][0]
        origin=item[d][0]
        out=routes[(origin,destination)][int(r[d])]
        print('%10s%10s%10s %5s km%3s' % (name,origin,destination,
                                                  out[0],out[1]))
                                                  
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
        mass=.056*size # metric tons
        # get trips
        trips=int(mass/cap)
        ## extract the destination id
        ent=outbound[0]
        ## assign mass to facility
        weight[ent].append(int(mass))
        print("weight is",weight)
       
        # Total price is the co2 kg of all outbound routes
        totalprice+=carbon_intensity*hv*mass*km_per_mile/(outbound[1]*mpg_ton)
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
            capfee+=100000
        if int(tot[i][0])>0:
            # penalize for decentralization
            cenfee+=100
            
    return totalprice+capfee+cenfee

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

