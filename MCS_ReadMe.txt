1_3 - Prep of mcs was not changed. It worked exactly as expected and matched with Orben's results. From UK millennium cohort study. This is run first. 

'ConfigRun.R'
Contains all the possible configurations that were ran. These configurations include different controls, sex-split data, scales, items, etc.
This can be run all at once to dump the table for each configuraiton. Sources to parameters_mcsJL.R

'parameters_mcsJL.R'
Contains all the labels necessary for formating and making the table readable and organized. Sources to 2_3 and 3_3

'2_3_sca_mcsJL.R'
Heavily modified from Orben's original script. Keeps all the core components such as the functions and original controls, x_variables, etc.
Modifications are put in multiple if statements to only change when configuration matches the if statement. These changes all depend on the type of configuration chosen.

'3_3_sca_analyse_mcsJL.R'
Modified from the original to output the datatables. Extended to contain all possible x_variables. 
