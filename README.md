# The impact of climate change on transmission season length: West Nile virus as a case study

Supporting code for Fay et al. (2026) "The impact of climate change on transmission season length: West Nile virus as a case study"

To Cite:

Fay R.L., Glidden C.K., Trok J.T., Diffenbaugh N.S., Ciota A.T., Mordecai E.A. (2026). The impact of climate change on transmission season length: West Nile virus as a case study. [DOI]

Please contact Jared Trok at trok@stanford.edu with any questions about the code.

# Scripts
This subdirectory contains the following scripts used to perform the main analysis:

- 1_BCSD.ipynb:
    - This notebook loads the gridMET and CMIP6 data from "/input_data/", bias corrects the CMIP6 data to match the distribution of gridMET data, spatially disaggregates the bias-corrected CMIP6 data to match the 4km gridMET resolution, then calculates the average daily temperature timeseries for each NY-state county from both the gridMET and BCSD-corrected CMIP6 data.
        - At one location in this notebook, it is necessary to run a command from the command line using the cdo and nco packages.
        - When prompted, one must also run the command "bash 1a_SD.sh" to spatially disaggregate bias-corrected climate model data to the 4km gridMET grid.
- 2_attribute_WNV_season_length.ipynb:
    - This notebook loads the county-level gridMET data and BCSD-adjusted climate model data, calculates the WNV season characteristics, and creates the figure panels used in the manuscript.
