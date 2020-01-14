
import sys, os
#print(sys.executable)
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import numpy as np
import pandas as pd
import datetime
from Figures import ReportFigures
from matplotlib import cm
from datetime import date
from datetime import timedelta
from dateutil.parser import parse
import matplotlib.dates as mdates
from dateutil.relativedelta import relativedelta
#%matplotlib inline

# Use USGS report styles
rf = ReportFigures()
rf.set_style()
sys.path.append('..')
python_exe = sys.executable

# calculate number of lines in file can be any gage file
num_lines = 0
with open(r".\output\Agwater1SW_high.ts9", 'r') as f:
    for line in f:
        num_lines += 1

# set dates for daily values; this date is simulation starte date
dates = []
for i in range(num_lines-1): 
    dates.append(datetime.date(2014, 10, 1)+datetime.timedelta(days=i))
    
# set start and end dates for plot 
startdate, enddate, plotdates = [], [], []
startdate.append(datetime.date(2016, 10, 1))
enddate.append(datetime.date(2018, 9, 30))

# calculate number of lines in file
num_lines = 0
with open(r".\input\seg1_high.tab", 'r') as f:
    for line in f:
        num_lines += 1

# set list of months for reading SFR inflows
# assumes values are one per month
nummonths = num_lines
date_list = list()
for i in range(0, nummonths):
    date_list.append(datetime.date(2014, 10, 1)+relativedelta(months=i))

#open segment 1 tabfile
fname1 = open(r".\input\seg1_high.tab","r")
fname2 = open(r".\input\seg1_low.tab","r")

seg1_high, seg1_low, date_plot = [], [], []
    
# Read flows
lines=fname1.readlines()
# set flows
i=-1
for line in lines:
    i=i+1
    if date_list[i]>enddate[0]:
        break
    if date_list[i]>=startdate[0]:
        seg1_high.append(line.split()[1])
        date_plot.append(date_list[i])
fname1.close()
# Read flows
lines=fname2.readlines()
# set flows
i=-1
for line in lines:
    i=i+1
    if date_list[i]>enddate[0]:
        break
    if date_list[i]>=startdate[0]:
        seg1_low.append(line.split()[1])
fname2.close()

# plot inflows
locator = mdates.MonthLocator()  # every month
# Specify the format - %b gives us Jan, Feb...
fmt = mdates.DateFormatter('%Y-%m')
fig, ax = plt.subplots(1, 1, figsize=(14, 6))
x = plt.gca().xaxis
x.set_major_locator(locator)
# Specify formatter
x.set_major_formatter(fmt)
ax.plot(date_plot,seg1_high,color='b', linewidth=1.0, label='High inflow hydrograph')
ax.plot(date_plot,seg1_low,color='r', linewidth=1.0, label='Low inflow hydrograph')
start, end = ax.get_xlim()
ax.xaxis.set_ticks(np.arange(start, end, 90.0))
ax.set_ylabel('Flow, in cubic feet per second')
ax.set_xlabel('Time')
# Set legend
handles, labels = ax.get_legend_handles_labels()
rf.legend(ax, handles, labels, bbox_to_anchor=(.9, 0.7))
plt.savefig(r'.\output\Fig5.png',dpi=300)

#open irrigation segment file again to read lines
fname = open(r".\output\Agwater1SW_high.ts9","r")
#set firstline for headers
firstline=fname.readline()
# store all data from file in lines
lines=fname.readlines()
y1_high, y2_high, y3_high, y4_high = [], [], [], []
y1_high_cum, y2_high_cum, y3_high_cum, y4_high_cum = [], [], [], []
# set variabes for plotting between startdate and enddate
i=-1
for line in lines:
    i=i+1
    if dates[i]>enddate[0]:
        break
    if dates[i]>=startdate[0]:
        plotdates.append(dates[i])
        y1_high.append(line.split()[4])
        y2_high.append(line.split()[5])
        y3_high.append(line.split()[6])
        
x1=np.array(y1_high,dtype=float)
#convert cubic feet per second to acre-ft/acre-day
x1=x1*5.76e-4
#y1_high=x1
y1_high_cum=x1.cumsum()
x2=np.array(y2_high,dtype=float)
x2=x2*5.76e-4
y2_high=x2
y2_high_cum=x2.cumsum()
x3=np.array(y3_high,dtype=float)
x3=x3*5.76e-4
y3_high=x3
y3_high_cum=x3.cumsum()
# sum up SW and GW            
y4_high=x2+x3 
y4_high_cum=x2.cumsum()+x3.cumsum()
# close irrigation segment file
fname.close()

#print out average annual NIWR
print(y1_high_cum[len(y1_high_cum)-1]/2)
print(y4_high_cum[len(y4_high_cum)-1]/2)

header=[]
# set headers for plot legend
header.append(firstline.split()[0])
header.append(firstline.split()[1])
header.append(firstline.split()[2])
header.append(firstline.split()[3])
header.append(firstline.split()[4])
header.append(firstline.split()[5])
header.append(firstline.split()[6])

#open irrigation segment file again to read lines
fname = open(r".\output\Agwater1SW_low.ts9","r")
#set firstline for headers
firstline=fname.readline()
# store all data from file in lines
lines=fname.readlines()
x, y1_low, y2_low, y3_low = [], [], [], []
# set variabes for plotting between startdate and enddate
i=-1
for line in lines:
    i=i+1
    if dates[i]>enddate[0]:
        break
    if dates[i]>=startdate[0]:
        y1_low.append(line.split()[4])
        y2_low.append(line.split()[5])
        y3_low.append(line.split()[6])
        
x1=np.array(y1_low,dtype=float)
x1=x1*5.76e-4
y1_low=x1
y1_low_cum=x1.cumsum()
x2=np.array(y2_low,dtype=float)
x2=x2*5.76e-4
y2_low=x2
y2_low_cum=x2.cumsum()
x3=np.array(y3_low,dtype=float)
x3=x3*5.76e-4
y3_low=x3
y3_low_cum=x3.cumsum()
# sum up SW and GW            
y4_low=x2+x3 
y4_low_cum=x2.cumsum()+x3.cumsum()
# close irrigation segment file
fname.close()

#print out average annual NIWR for 2 years
print(y1_low_cum[len(y1_low_cum)-1]/2)
print(y4_low_cum[len(y4_low_cum)-1]/2)

#open SW ET file to read lines
fname = open(r".\output\Agwater1SW_ET_high.ts9","r")
#set firstline for headers
firstlineet=fname.readline()
# set headers for plot legend
header2=[]
header2.append(firstlineet.split()[0])
header2.append(firstlineet.split()[1])
header2.append(firstlineet.split()[2])
header2.append(firstlineet.split()[3])
header2.append(firstlineet.split()[4])
header2.append(firstlineet.split()[5])
header2.append(firstlineet.split()[6])
# store all data from file in lines
lineset=fname.readlines()
xet, y1et_high, y2et_high, y3et_high = [], [], [], []
# set variabes for plotting between startdate and enddate
i=-1
for line in lineset:
    i=i+1
    if dates[i]>enddate[0]:
        break
    if dates[i]>=startdate[0]:
        #plotdates.append(dates[i])
#        xet.append(line.split()[0])
        y1et_high.append(line.split()[4])
        y2et_high.append(line.split()[5])
# close SW ET file
fname.close()

# cumulate ET
x=np.array(y1et_high,dtype=float)
x=x*5.76e-4
y1et_high = x
y1et_high_cum=x.cumsum()

# cumulate ET
x=np.array(y2et_high,dtype=float)
x=x*5.76e-4
y2et_high=x
y2et_high_cum=x.cumsum()

#print out average annual ET for 2 years
print(y1et_high_cum[len(y1_high_cum)-1]/2)
print(y2et_high_cum[len(y4_high_cum)-1]/2)

#open SW ET file to read lines
fname = open(r".\output\Agwater1SW_ET_low.ts9","r")
#set firstline for headers
firstlineet=fname.readline()
# store all data from file in lines
lineset=fname.readlines()
xet, y1et_low, y2et_low, y3et_low = [], [], [], []
# set variabes for plotting between startdate and enddate
i=-1
for line in lineset:
    i=i+1
    if dates[i]>enddate[0]:
        break
    if dates[i]>=startdate[0]:
        y1et_low.append(line.split()[4])
        y2et_low.append(line.split()[5])
# close SW ET file
fname.close()

# cumulate ET
x=np.array(y1et_low,dtype=float)
x=x*5.76e-4
y1et_low=x
y1et_low_cum=x.cumsum()

# cumulate ET
x=np.array(y2et_low,dtype=float)
x=x*5.76e-4
y2et_low=x
y2et_low_cum=x.cumsum()

#print out average annual ET for 2 years
print(y1et_low_cum[len(y1_low_cum)-1]/2)
print(y2et_low_cum[len(y4_low_cum)-1]/2)

# plot allowable irrigation periods
locator = mdates.MonthLocator()  # every month
# Specify the format - %b gives us Jan, Feb...
fmt = mdates.DateFormatter('%Y-%m')

fig, ax = plt.subplots(1, 1, figsize=(10, 4))

x = plt.gca().xaxis
x.set_major_locator(locator)
# Specify formatter
x.set_major_formatter(fmt)

ax.plot(plotdates,y1_high,color='b', linewidth=1.5, label=header[4])

ax.set_ylabel('Flow, in cubic feet per second')
ax.set_xlabel(header[0])


start, end = ax.get_xlim()
ax.xaxis.set_ticks(np.arange(start, end, 90.0))

# Set legend
handles, labels = ax.get_legend_handles_labels()
rf.legend(ax, handles, labels, bbox_to_anchor=(.9, 0.4))

#plt.tight_layout()
#plt.savefig('WR_irrig_allowed.pdf')

# blow up the fonts for this demo
plt.rcParams['xtick.labelsize'] = 8
plt.rcParams['ytick.labelsize'] = 8
plt.rcParams['axes.titlesize'] = 14

fig, axes = plt.subplots(2, 1, figsize=(10, 10))
axes = axes.flat
axes[0].plot(plotdates,y2_low,color='r', linewidth=1.5, label=header[5])
axes[0].plot(plotdates,y3_low,color='b', linewidth=1.5, label=header[6])
axes[0].plot(plotdates,y2et_low,color='g', linewidth=1.5, label=header2[5])

axes[1].plot(plotdates,y2_high,color='r', linewidth=1.5, label=header[5])
axes[1].plot(plotdates,y3_high,color='b', linewidth=1.5, label=header[6])
axes[1].plot(plotdates,y2et_high,color='g', linewidth=1.5, label=header2[5])

axes[0].set_ylabel('Acre-feet per acre')
axes[0].set_xlabel(header[0])

plt.ylabel('Acre-feet per acre')
plt.xlabel(header[0])

start, end = ax.get_xlim()
ax.xaxis.set_ticks(np.arange(start, end, 90.0))

rf.title(axes[0], 'Surface water and supplementary groundwater irrigation for low inflow hydrograph', subplot_prefix='A')

rf.axes_numbering(axes[1], format_x=True)
fmt = mdates.DateFormatter('%Y-%m-%d')
rf.title(axes[1], 'Surface water and supplementary groundwater irrigation for high inflow hydrograph', subplot_prefix='B')

# Set legend
handles, labels = axes[0].get_legend_handles_labels()
rf.legend(axes[0], handles, labels, bbox_to_anchor=(.9, 0.35))

# Set legend
handles, labels = axes[1].get_legend_handles_labels()
rf.legend(axes[1], handles, labels, bbox_to_anchor=(.9, 0.35))

plt.tight_layout()
plt.savefig('.\output\Fig6.pdf',dpi=300)

