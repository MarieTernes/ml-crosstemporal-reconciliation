READ ME to reproduce all the figures and tables for the Citi Bike Application from the paper "Cross-Temporal Forecast Reconciliation at Digital Platforms with Machine Learning" by Jeroen Rombouts, Marie Ternes and Ines Wilms forthcoming in International Journal of Forecasting

The folder contains all computer code to run our method, on any dataset the practitioner/researcher is choosing.
Regarding the data availability, please note however that in our paper we present two applications:
The first application for the last-mile delivery platform involves proprietary data from our industry collaborator which we are not allowed to share. The second application concerns the publicly available Citi Bike data. Therefore, the folder contains the data for Citi Bike and all scripts to reproduce all results related to Citi Bike.


The following software versions were used to produce the results:
- Specification of machine: AMD Ryzen Threadripper PRO 5965WX 24_Cores, 3801 MHz, 24 Core(s), 48 Logical Processor(s)
- R version 4.3.3 (2024-02-29 ucrt)
- Platform: x86_64-w64-mingw32/x64 (64-bit)
- Running under: Windows 10 x64 (build 19044)
- Please note: we noticed different results in the ML R packages (randomForest, xgboost, lightgbm) across different OS systems (Windows and MacOS, even after seed control). 
The main conclusions remained unchanged across different OS but in order to reproduce the results reported in the paper, the scripts need to be run on a Windows machine. 
We rerun the results on a Macbook Pro M1 macOS Sonoma 14.0. The differences between the two operations systems were overall small (smaller than 0.0002).

% Code %
The main folder "Code" contains the three subfolders: "data", "main", "sensitivity", detailed below. 
It also includes the R-scripts functions.R and forecast_evaluation_functions.R containing all main functions that are sourced in the scripts detailed below. 

% data %
This subfolder contains: 
- citibike.RData: the Citi Bike dataset that consists of demand for bicycle rentals across six H3 cells in New York City (NYC). 
	-- Content: The RData object contains two slots:
		--- citibike$citibike: the data for the six H3 cells and the aggregated NYC level.
		--- citibike$Cmatrix: the cross-sectional matrix mapping the bottom level series into the higher level ones.
- plots.R: 
	-- Content: Script to generate Figure 6 (right panel).
	-- Instructions for running: Select all to run the entire script at once. 
	-- Output: The script generates the pdf file Figure6.pdf.

% main % 
This subfolder contains:
- 1_RUN_citibike.R: 
	-- Content: Script to get the main results for the Citi Bike Application (Tables 6-7 and Tables A3-A4 in the Appendix). 
  	-- Instructions for running: Select all to run the entire script at once. 
	-- Run time of entire script: Around 14h hours, run times are also indicated separately for each line of code in the script.
	-- Dependencies: The remaining scripts in the folder "main" are sourced in the script 1_RUN_citibike.R:
		--- base_forecasts.R : get the base forecasts for Naive, ETS and SARIMA (needed as inputs for the ML or linear forecast reconciliation).
		--- base_forecasts_combinations.R : averages the base forecasts for Naive, ETS and Sarima. 
		--- linear-foreco.R : linear forecast reconciliation benchmarks.
		--- ML-foreco-main-results.R : machine learning forecast reconciliation (random forest, XGBoost, LightGBM).
		--- insample_naive_residuals.R : Needed as scaling factor when calculating the Mean Absolute Scaled Error (MASE) accuracy index.
		--- Tables_6-7.R : calculates the WAPE (Weighted Absolute Percentage Error) and returns Tables 6 and 7 from the main paper.
		--- Tables_A3-A4.R : calculates the MASE and returns Tables A3 and A4 from the Appendix.
	-- Output: The script will create a subfolder "NYC" containing all results saved in .RData objects.  
		   Additionally, the script generates the following main tables as .RData objects in the folder "main":
		--- Table6.RData, Table7.RData, TableA3.RData, TableA4.RData



% sensitivity %
This subfolder contains:
- 2_RUN_citibike-sensitivity.R: 
	-- Content: Script to get the sensitivity results for the Citi Bike Application (Tables B1 and Table B2 in Appendix). 
		    !!! This script can only be run after 1_RUN_citibike.R in "main" has run !!! 
  	-- Instructions for running: Select all to run the entire script at once. 
	-- Run time of entire script: Around 18h hours, run times are also indicated separately for each line of code in the script.
	-- Dependencies: !!! This script can only be run after 1_RUN_citibike.R in "main" has run !!! 
			The remaining scripts in the folder "sensitivity"  are sourced in the script 2_RUN_citibike-sensitivity.R: 
		--- linear-foreco-sensitivity-features.R: linear for the sensitivity analysis on the temporal frequencies. 
		--- ML-foreco-sensitivity-features.R : machine learning forecast reconciliation for the sensitivity analysis on the features matrix.
		--- ML-foreco-sensitivity-frequencies.R: machine learning forecast reconciliation for the sensitivity analysis on the temporal frequencies. 
		--- Tables_B1.R : calculates the relative WAPE (complete features / compact features) and returns Table B1 (split into the results for the H3 cells "TableB1_H3cells" and the market "TableB1_market") from the Appendix.
		--- Tables_B2.R : calculates the relative WAPE (using all ten temporal frequencies / using only temporal frequencies of interest) and returns Table B2 from the Appendix.
	-- Output: The script will create a subfolder "NYC" with two subfolders 
				"cstempall": containing the sensitivity results on the features matrix saved in .RData objects. 
				"subfreq": containing the sensitivity results on the temporal frequencies saved in .RData objects. 
		   Additionally, the script generates the following tables as .RData objects in the folder "sensitivity":  
		--- TableB1_H3cells.RData, TableB1_market.RData, TableB2.RData
