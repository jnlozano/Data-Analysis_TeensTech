'MergeMTF_.R'
This script attempts to pull all the data together in a logical way. However, it does not match up with Orben's data. This is detailed more in the writeup. The data we used is pulled directly from the ICPSR website: https://www.icpsr.umich.edu/icpsrweb/ICPSR/series/35 (12/2020) (fairly certain about this) 
This is a brief detailing of my logic:
Read 3 versions of the data. 01,02,03. 
Select variables from each respective version that containss the variables
Add NA columns to each dataset to have matching rows
Add all variables to a single dataframe
Rename variables to unified names of variables.
Repeat for years 2008-2011.

d81 and d82 change with years 2008-2011 
d83 remains consistent for the most part  

'5_3_comp_mcs.r'
Only 5_3 was changed from Orben's original set of code for MTF. Our attempts fell flat due to the data not matching up. 
