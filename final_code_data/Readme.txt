This folder is for the final data for share.  Include 3 raw data file, 3 codebooks, and 3 R scripts for the pilot data:

1. HPP_Pilot_PA_Calc_Score.r

-- Purpose:   calculating the score for each scale;

-- Input:       Data_Raw_HPP_Pilot_PA_Share.csv

-- Output:    Data_Sum_HPP_Pilot_PA_Share.csv (processed data for analysis in HPP)

-- Sharable:  Yes

2. HPP_Pilot_Mturk_Calc_Score.r

-- Purpose:   calculating the score for each scale;

-- Input:       Data_Raw_HPP_Pilot_MT_Share.csv

-- Output:    Data_Sum_HPP_Pilot_MT_Share.csv (processed data for analysis in HPP)
	    Data_Sum_HPP_Pilot_All_Share.csv (merged summary data, for analysis in HPP)

-- Sharable:  Yes

3. HPP_Multi_Site_Calc_Score.r

--Purpose: calculating the scores for different scales 

--Input:      Data_Raw_HPP_Multi_Site_Share.csv;

--Output:   Data_Sum_HPP_Multi_Site_No_Share.csv (sum data for late analysis, can't be shared)
	  Reliability_HPP_Multi_Site_Share.csv (reliabiltiy information for each scale, can be shared)
	  Descriptives_HPP_Multi_Site_Share  (descriptives for each scale, can be shared)