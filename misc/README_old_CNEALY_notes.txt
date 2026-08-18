-------------------------TO DO------------------------------
- switch worm_plot_module to filter by Sys.time (instead of today) once operating
- update to using bslib::layout_columns for plotting timeseries grids instead of using fluidrow() in the module UI (see county_plot_module.R example)
- all data are processed to MDT
	- therefore, 24hr "outlook" tab is all MDT, as it's aimed towards summer wildfire smoke forecasting
	- the hourly data forecast will switch between MDT and MST (using 
		- EXCEPT VENT_WINDOW WILL BE MDT
		- NEED TO FIX THIS


---------------------------NOTES (5/30/2025 RUNDOWN)--------------------------
Outlook and Trends
- AQI outlook is county area-averaged near-surface smoke; 24 hours for 'tomorrow' and 18 hours for 'today' (because 48-hr HRRR run is valid at 6am MST/12 UTC every day)
  - 6pm/00 UTC HRRR 48-hr fcst is also produced, but I'm not running it because less useful
- VENT_RATE looks at county area-averaged max 1hr value (similar to WA ECY dashboard); uses UW breaks
- all trend variables are with reference to 'tomorrow' (since the trend is really only useful for writing forecasts and less so making AQAs or burn approvals)
- fire table pulls both CAN (acres converted from hectares) and US active fires (>5000 acres non-MT; >100 acres MT); only BC, AB, WA, ID, OR, CA, NV, WY fires included

Hourly Forecast
- displays all hours of 12 UTC HRRR 48-hour run
- uses raw values for all variables (no trends)

County Forecast Timeseries
- all variables represent county area-averaged values

Worm Plots
- Current monitor values and previous 48 hours
- Option to display 24-hr running averages
  - any sites that are currently USG or above for 24-hr running avg are automatically added to the AQA county list if an AQA is made using the 'today' outlook (not added to the 'tomorrow' outlook AQA because of uncertainty)
  - oldest hours don't have running average because average requires 24 hours of data

Model Performance
- option to compare stats for the 0-day and 1-day lead times
- unlike the 'today' (i.e., 0-day lead) AQI outlook, the AQI difference tile plot backfills the 6 hours of data preceding the first valid hour with the previous day's model run data for those hours
  - therefore, both the 0-day and 1-day HRRR averages are 24 hours (the 0-day utilizes 6 hours of the 1-day's data)
- changing the lead time also changes which data are plotted for the timeseries and bias plots

AQA Text Product
- automatically fills out AQA txt file that
  - lists counties subject to alert
  - lists reason for alert (brief explanation of conditions)
  - lists current conditions at monitors
  - lists AQI category meaning and recommended action
  - lists Today's Air URL
- expiration time defaults to 8AM but can be changed
- if 'today' AQI outlook used, counties with HRRR 18-hr averages (today) >USG AND monitor 24-hr running averages >USG are subject to alert
  - expiration date defaults to 'tomorrow'
- if 'tomorrow' AQI outlook used, just counties with HRRR 24-hr averages (tomorrow) >USG are subject to alert
  - expiration date defaults to 'day after tomorrow'
- current conditions listed for any monitor that is moderate or above
- AQI category and recommended action listed for each AQI category where a monitor is above 'good'
		



---------------------------OLD NOTES---------------------------
Done:
- Get other variables
	- WIND_1hr_max_fcst (10m_above_ground, m/s)
		data type: f4 ?check
	- TMP (2m_above_ground, K) 
		data type: f4
	- RH (2m_above_ground, %) 
		data type: f4
	- FRP (surface, CFNSF, W/m2) 
		data type: f4
	- GUST (surface, m/s) 
		data type: f4
	- PBL height (surface, HPBL, m)  
		data type: f4
	- MASSDEN data type: f8

- VENT_RATE (per Brandon's code)
	- Very Poor = <235 m2/sec
	- Poor = <2350
	- Marginal = < 4700
	- Good = >= 4700

Sources:
- NOAA (HRRR data)
- CWFIS and NIFC (fire data)

Other notes:
- model_performance
  - AQI_HRRR = 24hr avg HRRR monitor point forecast
  - accuracy = point vs point comparison (not point vs county avg)