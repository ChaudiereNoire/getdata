# getdata
Getting and Cleaning Data assignments

This repository contains the script run_analysis.R, which reads an dataset of experimental data from
an experiment having to do with the motion detection capabilities of the Samsung.

The project runs in R and expects to find the dataset from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip unzipped in the working
directory.  It has one argument, BuildRawFile, which defaults to FALSE.  If set to true, the script attempts to
parse the raw data into tidy files using for loops - it is very slow.  In all case the script produces two text
files, getdata-project-data.txt and getdata-project-tidydata.txt, in the working directory.
