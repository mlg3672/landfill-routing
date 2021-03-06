#Energy Materials Routing Problem 

## Purpose
1. To solve classical location-allocation problem 
2. To efficiently route materials from starting point (installation) to destination (landfills)
2. To reduce carbon footprint / climate impacts of routing
3. To explore the relationship between system network and environmental/climate-related impact of routing activities

## Approach
### Part I. 
mine data for all landfills and installation locations
delineate locations into regions based on MSW import/export patterns
reduce landfill points to ten per county
reveal cluster pattern of pv in each region
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
A. distributed vs. centralized network 
B. boundaries: state, county, region
B. vehicle type 
C. recycling technology mandates
D. change in route - impact on vulnerable communities, traffic congestion, local ecosystems 

Other questions: 
1. what is the current carbon footprint of each region, state, county? How does this change the balance?
2. Is cooperation in routing necessary or beneficial?
3. How does each region compare?

Goal : complete writing manuscript by Tuesday
To Do:
Tuesday 
Monday start early!
- add to methodology missing sections
- plan graphics
- quick literature review
- things don’t need to be perfect just done
- use python output to make a plot for region 4
- does the python output make sense
- how to visualize the output?
- how to improve the code to make it work easier for updates?
- do all regions with existing data
- are any of the pv centers in weird locations?
- should i update for future pv adoption?
- make the s curve
- clear conclusions
- email Bose resume for sustainability corporate position 
