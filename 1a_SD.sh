#!/bin/bash

## Spatially disaggregate bias-corrected climate model data to 4km gridMET grid ##

## * note: requires both cdo and nco to be installed * ##
## * note: consider parallelization to reduce runtime * ##

## use nearest-neighbor regridding to avoid cold bias in coastal regions due to zeros over the ocean ##
cdo setmisstonn "../processed_data/gridMET_4km_tas_climatology.nc" "../processed_data/gridMET_4km_tas_climatology_NN.nc"

## bilinearly interpolate bias-corrected residuals to 4km grid, add 4km gridMET climatology, then extract NY-state subregion ##
FILES="../processed_data/BC_GCM_tas_residuals_*.nc"
for f in $FILES; do
    if [[ ! $f == *"_4km."* ]]; then
        f_4km=${f%.nc}_4km.nc
        outfile=${f_4km/"_residuals_"/"_"}
        outfile_subreg=${outfile/"BC_GCM_"/"BCSD_"}
        if [ -f $outfile_subreg"" ]; then
            echo $outfile_subreg "already exists..."
        else
            echo $f 
            echo $f_4km 
            echo $outfile_subreg
            cdo -b F32 remapbil,"../processed_data/gridMET_4km_tas_climatology_NN.nc" $f $f_4km
            cdo -b F32 add $f_4km "../processed_data/gridMET_4km_tas_climatology_NN.nc" $outfile
            ncks -O -d lon,-80.0,-71.0 -d lat,40.0,45.5 $outfile $outfile_subreg
            rm $f_4km $outfile
        fi
    fi
done