# SMSD
The replication materials for “Structural Monetary Segmentation in Daily Frequency”

This file contains the replication materials for “Structural Monetary Segmentation in Daily Frequency” by Hao Liu, May 2019.


1.	The first step of the replication process is making all the data files with the scripts in the same folder archive in your environment. The installed R packages are necessary in the programs. 

2.	The R script, “BondEventsXTS.R” and the yield curve data, “YieldCurve.csv” would replicate the results of the change point estimation (Figure 3). 

3.	The R script, “spillover.R” is the main program to visualize the IRFs in dimensional frequencies using all the replication data, “US_CMT_rates.csv”, “Un_Ch.csv”, “CISS.csv”, “YieldCurve.csv”, “Money_Supply.csv”, “Stock_Market.csv”, and “MacroUncertaintyTo Circulate.csv” (Figure 4). 

4.	The R script, “split.R” is preparing for the robustness analysis with the time period after the financial crisis with the same replication data in the last procedure (Figure A.5). The data split is transforming Data Manipulation with dplyr package. 

With the Cairo package, the high-quality printable graphs are creating with the scripts in the same file folder where you preliminarily save the scripts and replication files. 

