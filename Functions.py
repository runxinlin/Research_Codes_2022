import numpy as np
import pandas as pd

# Replace columns that have consecutive (n) 0s with NAs
def replacer(l,target_val=0,replace_val=np.NaN,repeat_max=10):
    #repeat_max = n (consecutive 0s over 10 times will be replaced with NAs)
    counter = 0
    new_l = []
    for e in l:
        if e == target_val: counter += 1
        else:
            counter = 0

        if counter > repeat_max:
            new_l.append(replace_val)
        else:
            new_l.append(e)

    return new_l

# Deal with 0s and NAs in a dataframe
tmp = pd.DataFrame()
tmp = tmp.loc[:, (~np.isnan(tmp).any(axis=0))].astype(np.float64) # remove columns that contain any nas
tmp = tmp.loc[:, (tmp != 0).all(axis = 0)].astype(np.float64) # remove columns that contain any 0s

tmp = tmp.loc[:, (~np.isnan(tmp).all(axis=0))].astype(np.float64) # only remove columns that full of nas
tmp = tmp.loc[:,(tmp != 0).any(axis=0)].astype(np.float64) # only remove columns that full of zeros
