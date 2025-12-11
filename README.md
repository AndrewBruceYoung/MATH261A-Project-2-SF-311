# MATH261A-Project-2-SF-311

**Author:** Andrew Young  
**Course:** MATH261A  
**Submission date:** 2025-12-10

## Overview

This project analyzes San Francisco 311 *Street & Sidewalk Cleaning (SSC)* requests (2018–2019) to ask:
Do request type, weekday, and time-of-day help explain how long a request remains open?  
The outcome is `duration_hours` (close time − open time, in hours). I fit an OLS multiple
linear regression with categorical predictors for `request_type`, `weekday`, and `tod_bin`
(Night/Morning/Afternoon/Evening). 

## Repository structure

analysis/
- Math261A-Project2-Analysis.R                      # data import, cleaning, model fit, plots

data/
- raw_sf311.csv      # raw data file
- cleaned_sf311.csv  # cleaned data file

paper/
- Math261A-Project2-Paper.Rmd                       # paper source
- Math261A-Project2-Paper.pdf                       # knitted paper 
- references.bib                                    # BibTeX references

README.md

.gitignore

## External Sources

https://data.sfgov.org/City-Infrastructure/NBC-FY2018-2019-311-SC-Calls/pk4y-mgyw/about_data # for data

https://www.rdocumentation.org/ # for coding help

https://stackoverflow.com/questions # for coding help

## License statement

The dataset is provided via the California Open Data Portal, which lists a license for each resource on the landing page.
