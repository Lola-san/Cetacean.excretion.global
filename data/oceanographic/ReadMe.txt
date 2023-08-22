Data was downloaded from https://neo.sci.gsfc.nasa.gov/. Tab "OCEAN"
We chose July 2018 for both chlorophyll and sst, for sst we chos the MODIS data (2002+). 
https://neo.gsfc.nasa.gov/view.php?datasetId=MYD28M&date=2018-07-01

Then, we choose the month we want (July 2018), and we press the button "Download Raw Data". You must have
a free account logged in to download the data.

I am not sure the date are the ones we want, does not seem to change when we change the month! 

For chlorophyll, it does not seem that the name of the file contains the true date: there is the year, 
but for the month format is weird. And it does not change when we ask for another month. So well well, 
not so good! 

For sst, it seems that we just have December month, 2021 here! 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
UPDATE 2022 March, 3 
Found another way I think

Website https://oceancolor.gsfc.nasa.gov/l3/order/

Go to Data --> Browse data --> L3 Browser --> extract or Download data
Then select Product status -> Standard
Sensor -> MODIS-Aqua
Product -> Chloro^phyll concentration / SST (11micro daytime)
Period -> Annual (makes more sense)
Resolution -> 4km
Start-Date -> 2021-01-01
End Date -> 2021-12-31
Type -> Mapped
Data Retrieval method -> download
This leads to a page with a list of name file, one per year. Copy the link corresponding to year 2021 and paste 
on another page. 

https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20210012021365.L3m_YR_CHL_chlor_a_4km.nc
https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/AQUA_MODIS.20200101_20201231.L3m.YR.SST.sst.4km.nc

It should get downloaded! 
