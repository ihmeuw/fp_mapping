# Geospatial estimates of family planning indicators

This repo contains cleaned code used to generate estimates of six family planning indicators (CPR, mCPR, tCPR, met need, unmet need, and intention to use contraception) for women ages 15-49 in Burkina Faso, Kenya, and Nigeria from 2000 to 2020. 

The code is organized as follows: 
* **data/** -- code used to extract and process individual-level survey data 
* **mbg/**
  * **functions/** -- assorted functions
  * **collapse/** -- code used to collapse individual-level survey data to calculate prevalence by cluster (point) or area (polygon), then crosswalk (if appropriate) 
  * **pipeline/** -- code used to run all modeling stages, post-processing, model validation, and diagnostic plots 
  
