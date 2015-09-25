from FuncDesigner import *
from openopt import MINLP
import numpy as np
# two customer, one facility problem
vol, dump = oovars('vol','dump') # a is mass sent to LF, b is mass recovered at landfill, c is unrecovered
op = oovar('op', domain = np.array([1, 0])) # domain should be Python list/set/tuple of allowed values
startPoint = {vol:np.array([100,100]), dump:np.array([20,30]), op:np.array([0])} # however, you'd better use numpy arrays instead of Python lists

# variable attributes
cap = np.array([400])           # facility capacity per ton
demand = np.array([200,200])    # ton
elec = 5                        # CO2 from electricity per ton
ghg = 19.54                     # CO2 from transport per mile
dist = np.array([ 55, 15])      # distance in miles between pv and mrf
cost = 0                        # social cost of dumping

f = sum(sum(op*dist))*ghg + sum(op*vol)*elec + sum(dump)*cost  # min ghg impact
constraints = [ op*(vol + dump) == demand , op*sum(vol) < cap  ,vol >= 0]


p = MINLP(f, startPoint, constraints = constraints)
r = p.minimize('branb', nlpSolver='ralg', plot=0, discrtol = 1e-6, xtol=1e-7)
vol_opt,  dump_opt, op_opt = r(vol, dump, op)
# or any of the following: 
# a_opt,  b_opt, c_opt,d_opt = r(a), r(b), r(c),r(d)
# r('a'), r('b'), r('c'), r('d') (provided you have assigned the names to oovars as above)
# r('a', 'b', 'c', 'd')
# a(r), b(r), c(r), d(r)
print(vol_opt, dump_opt, op_opt)