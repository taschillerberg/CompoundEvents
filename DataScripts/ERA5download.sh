#!/bin/bash

# Download the data
python fetch_cds_datasets.py

# hr to day and regrid
for file in *.nc;do

	if [ -f "regrid360x180_"${file} ]; then
		echo Already completed regridding $file

	else
		new_file=regrid360x180_day_${file}
		# cdo -daymax -remapbil,r360x180 $file $new_file #To find the maximum daily temperature
		cdo -b F64 -daysum -remapbil,r360x180 $file $new_file #To find the total daily precipitation
		# cdo -dayavg -remapbil,r360x180 $file $new_file #To find the daily average soil moisture
	fi
done