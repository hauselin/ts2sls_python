#%%
import pandas as pd
import numpy as np
from copy import deepcopy
from ivregress import ivregress_2sls, ts2sls


np.set_printoptions(suppress=True)

#%%

# Creating the demo data
df1 = pd.read_csv("auto_data.csv", low_memory=False)
df1["weight2"] = df1["weight"] * df1["weight"]
df2 = deepcopy(df1)
df1["mpg"] = np.NaN

#%% ts2sls - single instrument (not yet implemented in R)

S1 = df1  # sample 1 - this doesn't have the endogenous variables
S2 = df2  # sample 2 - this doesn't have the target variable

y_var = "price"
regs = ["weight", "mpg"]
ev = ["mpg"]
inst = ["headroom"]
result = ts2sls(S1, S2, y_var, regs, ev, inst)
print(result)

#%% iveregress - single instrument (implemented in R)

S = df2
y_var = "price"
regs = ["weight", "mpg"]
ev = ["mpg"]
inst = ["headroom"]
result = ivregress_2sls(S, y_var, regs, ev, inst)

#%%
