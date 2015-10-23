#Energy Materials Routing Problem 

## Purpose
1. To solve classical location-allocation problem 
2. To efficiently route materials from starting point (installation) to destination (landfills)
2. To reduce carbon footprint / climate impacts of routing
3. To explore the relationship between system network and environmental/climate-related impact of routing activities

## Approach
### Part I. 
mine data for all landfills and installation locations
record shortest road distance between local points 
make some assumptions about composition
make some assumptions about recycling technology at each site
make some assumptions about transportation modes, capacity

### Part II.
non-linear optimization of routing network

variables include:
vehicle mpg
vehicle weight / capacity
fuel type
modal share
installation efficiency
geographic groups - county, state, regional
material composition
recycling technology - limited, 0-100%

### Part III. 
scenario analysis - policies to influence climate impacts
A. increasing parking cost (e.g. increase per trip cost)
B. green procurement 
C. improving public transport (e.g. decrease bus time, expand bus routes)