# individual_fx

This repository contains the data and programs needed to reproduce the results of the paper "Modeling the Age Pattern of Fertility: An Individual-Level Approach".

The initial step is to run compute/estimation/compute_ref_tables.R to obtain the reference tables. 
This is the most time-consuming step. As a rough guide, the computation of a reference table with 10^7 simulations too aprox. 30 hours, computing in parallel in 100 cores.

Once the tables are computed, everything else follows from compute/main.R    