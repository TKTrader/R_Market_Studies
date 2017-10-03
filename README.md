# Market Studies
Some examples of charts that I created in R for the purpose of analyzing different instruments (mainly ZC, ZW, ZS, CL) with regard to day types, high and low breaks; these charts were created for context and context alone in my own intraday/day trading. 

As the programs were written for my personal use to answer or examine specific questions on daily price action, they are not "properly" commented or formatted for industry/3rd party consumption.  

Key:
CL - Oil
ZW  - Wheat
ZS - Soybeans
ZC - Corn
ON - Overnight
RTH - Regular Trading Hours
OAIR - Open Auction in Range
OAOR - Open Auction Out of Range
IB - Initial Balance (typically first hour of trading, establishing day's context)
IBL - Initial Balance Low
IBH - Initial Balance High
MFE - Maximum Favorable Excursion (typically in ticks, except for CL range studies)
MAE - Maximum Adverse Excursion
zigzag - zigzag function; value for each instrument determined using 2 tick renkos and statistical analysis of average zigzag based on historical intraday distributions in relation to timeframe
HVN - High volume node  - high volume auction area, price attractor/magnet
LVN - low volume node  - low volume price area, typically repellent or sign that price discovery is taking place as we move to a new auction zone (new HVN node)

Example of question for analysis in conjunction with chartsand volume profiles:

For wheat (ZW) on an intraday basis, what is the expectation for price action follow through (momentum or mean reversion) based on a break of the IBH?  
In what half hour does this break typically occur? 
Given a break in the IBH and a higher ONH, what percentage of IBH breaks can expect to also break the ONH? 
Given a break in the IBH and failure, how often statistically will price action then consolidate at an HVN?  Or break the IBL?
Will price typically revert within range by end of day?  
Does a higher RSI > 70 increase expectation for followthrough and further range extension?

