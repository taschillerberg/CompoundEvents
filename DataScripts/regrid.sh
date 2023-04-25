#!/bin/bash

#place this scriptwhere you see all the cmip folders
#home_dir="/mnt/c/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/CMIP6_historical"

#for dir in "$home_dir"/*;do
#for dir in */;do
#cd $dir

for file in *.nc;do

if [[ "$file"  == *"regrid360x180_"* ]]; then
  echo Already completed regridding $file
else
  new_file=regrid360x180_${file}
  cdo remapbil,r360x180 $file $new_file
fi;done
#done
