import time
import random
import math
import pandas as pd
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import numpy as np
# to do : 
#         3. visualize results
#            A. map: red points start, green pts end
#            B. pie chart share of facilities co2
#            C. bar chart share of facilities mass


# constants
carbon_intensity=2237. #g co2 per MMBTU
hv=0.128450 #MMBTU per gal diesel
mpg=11 #miles per gal
cap=10 #30 truck capacity in metric tons (20,000 - 60,000 lbs)
km_per_mile=1.609 #km per mile
recov=92/.160 #.02# recycling energy kWhel/kWp for 1.125 m2 recovered
recyl= 0 # .97 percent recovered
size_to_mass = .0917 # 15.6 kg/ 170 W in metric tons
ton_to_lbs=2000
MWh_to_kWh=1000
alum = 1.84 # kg aluminum per m2 module
meters = 1.125/160 # meters squared per watt peak
recov_al = 31e6 * 2.7778e-7 # recycling energy kWh/kg of aluminum
shred = 0.34e6*2.778e-7 # kWhel per kg mass
insol=1700
eff=0.24
PR=0.8
efactors = {}
with open('emissionfactors/emissionfactors_gkwh.csv') as f:
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
with open('rev_codeR1.txt') as f:# reading routes for optimization purposes df
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
    item.append((d[0], routes[d][1][3]))
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

def writeresults(r,filename):
    header = np.array(['origin','dest','fac','f_dist','size','state', 'mass','realdist'])
    data = np.zeros(shape=(len(r),8),dtype=object)
    for d in range(int(len(r))):
        origin=item[d][0]
        size=item[d][1]
        mass = size_to_mass*size
        trips = math.ceil(mass/(cap))
        out=routes[(origin,destination)][int(r[d])]
        realdist= trips*(out[1])+int(out[2])
        data[d] = [origin,destination,
                 out[0],int(out[1]), 
                 int(out[3])/1000, 
                 out[4], 
                 int(mass),int(realdist)]
    np.savetxt(filename, data,delimiter=" ",newline="\n",fmt='%s')
   

                
def printschedule(r):
    # input schedule for outbound route of each item
    print('%10s %5s %5s %3s %3s %3s %3s %3s' % ('origin','dest',
                                                  'fac','f_dist','size', 
                                                 'state', 'mass','realdist'))
    for d in range(int(len(r))):
        origin=item[d][0]
        size = item[d][1]
        mass = size_to_mass*size
        trips = math.ceil(mass/(cap))
        out=routes[(origin,destination)][int(r[d])]
        realdist= trips*out[1]+out[2]
        print('%10s%5s %5s %3skm %5sMW %3s %3smt %3skm' % (origin,destination,
                                                  out[0],int(out[1]), int(out[3]/1000), out[4], int(mass),int(realdist)))
                                                  
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
    co2=0
    ch4=0
    no2=0
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
        mass=size_to_mass*size #  15.6 kg/170W * kW in mt
        Al = alum * size * meters * 1000 # kg of aluminum
        #print('proportion of aluminum is: ' + str(Al/1000/mass) )
        # get trips
        trips=math.ceil(mass/cap)
        
        ## extract the destination id
        ent=outbound[0]
        # emission factor of state where facility located
        state=outbound[4]
        # find recycling and shredding energy
        energy = (size*recov + Al*recov_al)*recyl
        shrd_enrgy = size*size_to_mass*(1-recyl)*shred*1000
        
        #find emissions by ghg gas
        co2+=efactors[(state)][0][0]*(energy+shrd_enrgy) # grams co2  
        ch4+=efactors[(state)][0][1]*(energy+shrd_enrgy)
        no2+=efactors[(state)][0][2]*(energy+shrd_enrgy)
        
        ## assign mass to facility
        weight[ent].append(int(size))
           
       
        # Total price is the co2 kg of all outbound routes
        totalprice+=(carbon_intensity*hv*(trips*outbound[1]+outbound[2]))/(km_per_mile*mpg)#grams
        
    for i in weight: 
        k =0
        for j in weight[i][:]:
            k+=j
        tot[i].append(int(k))
    s= 0
    for i in tot: 
        s+=sum(tot[i])
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
    if recov == 0: # should pv be recovered?
        elec = 0
    else: 
        elec=co2+ch4*25+no2*298 # g CO2-equ
    
    #print('grams of CO2-equ per kWp is: ' + str((totalprice+elec)/s)) 
    #print('or per m2 is: '+ str((elec)/(s*meters*1000)))
    print('g CO2-equ per kWh is: ' + str(round((totalprice+elec)/(s*insol*PR*eff*meters*30/1000),2)))
    score = np.array([totalprice,capfee,cenfee,elec,co2,ch4,no2])
    np.savetxt('score.csv',score,delimiter=" ")
    
    return totalprice+capfee+cenfee+elec

def printcapacity(sol):
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
        ## extract the destination id
        ent=outbound[0]
        #get the size
        size = item[d][1]

        ## assign mass to facility
        weight[ent].append(int(size))
        
   
    for i in weight: 
        k =0
        for j in weight[i][:]:
            k+=j
        tot[i].append(int(k))
    return(tot)
    
def randomoptimize(domain,costf):
    # costf refers to the cost function
    best=9999999999999
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

