# -*- coding: utf-8 -*-
import sklearn
from pandas import Series, DataFrame
import pandas as pd
import numpy as np
import os
from scipy import stats
from pandas.stats.api import ols
import statsmodels.formula.api as sm
import statsmodels.api as smg
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
import sklearn.metrics as sl
from sklearn.decomposition import PCA
from pylab import *
import seaborn as sns
import json
import plotly
from plotly.offline import download_plotlyjs, init_notebook_mode, iplot
from plotly.graph_objs import *

#Function to calculate 2010 data using other year data
def extrapolate_2010(col1,col2,year1,year2,pop):
    val_2010=(col1+(col2-col1)*(2010-year1)/(year2-year1)).round(0)
    return val_2010*1000/pop
    
#Function to assign metro and non-metro names    
def metro_name(clus_num):
    clus_map = {1: "Metro", 0: "Non-metro"}
    return clus_map[clus_num]



#Read and clean the file for spaces
xls = pd.ExcelFile('DataDownload.xls')
main=xls.parse('Supplemental Data - County')
main.columns=main.columns.str.strip()
#Import County code, name and state as the first dataset
main=main.rename(columns={"FIPS Code": "FIPS"})
df_base=main[['FIPS','County Name','State']]

#Import the columns required for analysis
cols = pd.read_excel('Columns.xlsx')
#Import state names required for analysis
states=pd.read_csv('States.csv')

#Append the columns required to the initial dataset
for sheet in (cols['Sheet']).unique():
    sheet_df=cols[cols['Sheet']==sheet]
    col_name=list(sheet_df['Column Name'])
    col_name.append('FIPS')
    sheet_req=xls.parse(sheet,na_values='<Null>')
    sheet_req.columns=sheet_req.columns.str.strip()
    sheet_req=sheet_req.rename(columns={"FIPS Code": "FIPS"})
    req_df=sheet_req[col_name]
    req_df=req_df.drop_duplicates()
    df_base=pd.merge(df_base,req_df,how='left')
 
#Merge to get the state name    
df_base=pd.merge(df_base, states, left_on="State", right_on="Abbrev",how='left')
colorder=df_base.columns.values

#Print the columns and number of null values
df_base.isnull().sum() 
#Extrapolate columns which do not have 2010 data
df_base['PCT_OBESE10'] = ((df_base['PCT_18YOUNGER10']*df_base['PCT_OBESE_CHILD11']) + ((100-df_base['PCT_18YOUNGER10'])* df_base['PCT_OBESE_ADULTS10']))/100
df_base['OBESE10']=df_base['PCT_OBESE10']*df_base['2010 Census Population']/100
df_base['DIABETES10']=((100-df_base['PCT_18YOUNGER10'])* df_base['PCT_DIABETES_ADULTS10'])*df_base['2010 Census Population']/10000
df_base['RECFACPTH10']=extrapolate_2010(df_base['RECFAC07'],df_base['RECFAC12'],2007,2012,df_base['2010 Census Population'])
df_base['FFRPTH10']=extrapolate_2010(df_base['FFR07'],df_base['FFR12'],2007,2012,df_base['2010 Census Population'])
df_base['FSRPTH10']=extrapolate_2010(df_base['FSR07'],df_base['FSR12'],2007,2012,df_base['2010 Census Population'])
df_base['SNAPSPTH10']=extrapolate_2010(df_base['SNAPS08'],df_base['SNAPS12'],2008,2012,df_base['2010 Census Population'])
df_base['WICSPTH10']=extrapolate_2010(df_base['WICS08'],df_base['WICS12'],2008,2012,df_base['2010 Census Population'])
df_base['GROCPTH10']=extrapolate_2010(df_base['GROC07'],df_base['GROC12'],2007,2012,df_base['2010 Census Population'])
df_base['SUPERPTHC10']=extrapolate_2010(df_base['SUPERC07'],df_base['SUPERC12'],2007,2012,df_base['2010 Census Population'])
df_base['CONVSPTH10']=extrapolate_2010(df_base['CONVS07'],df_base['CONVS12'],2007,2012,df_base['2010 Census Population'])
df_base['SPECSPTH10']=extrapolate_2010(df_base['SPECS07'],df_base['SPECS12'],2007,2012,df_base['2010 Census Population'])
df_base['FastFood10']=df_base['FFRPTH10']*df_base['2010 Census Population']
df_base['FullFood10']=df_base['FSRPTH10']*df_base['2010 Census Population']
df_base['Recfac10']=df_base['RECFACPTH10']*df_base['2010 Census Population']
df_base['AdultObese']=((100-df_base['PCT_18YOUNGER10'])* df_base['PCT_OBESE_ADULTS10'])

#Find Obese adult population in United States
df_obstate=df_base.groupby(['State']).sum()
df_obstate['PctAdultObese'] = df_obstate['AdultObese']/df_obstate['2010 Census Population']*100
df_obstate.to_csv('State_Obesity.csv', sep=',')


#Draw a heat map
scl = [[0.0, 'rgb(26,152,80)'],[0.2, 'rgb(145,207,96)'],[0.4, 'rgb(217,239,139)'],\
            [0.6, 'rgb(254,224,139)'],[0.8, 'rgb(252,141,89))'],[1.0, 'rgb(215,48,39)']]


data = [ dict(
        type='choropleth',
        colorscale = scl,
        autocolorscale = False,
        locations = df_obstate.index,
        z = df_obstate['PctAdultObese'].astype(float),
        locationmode = 'USA-states',
        marker = dict(
            line = dict (
                color = 'rgb(0,0,0))',
                width = 2
            )
        ),
        colorbar = dict(
            title = "Color Legend"
        )
    ) ]

layout = dict(
        title = 'Adult Obesity Distribution in United States',
        geo = dict(
            scope='usa',
            projection=dict( type='albers usa' ),
            showlakes = False,
            lakecolor = 'rgb(145,191,219)',
        ),
    )

fig = dict( data=data, layout=layout )
plotly.offline.plot(fig)



#Drop the columns which do not have obesity values

df_main=df_base.dropna(subset=['PCT_OBESE_ADULTS10','PCT_OBESE_CHILD11'])


#1. Find the top and least 5 Obese and diabetic states
#Aggregate data to state level and find the obesity and diabetes percentages
df_state=df_main[['State Name','OBESE10','DIABETES10','2010 Census Population','State']].groupby(['State Name','State'], as_index=False).sum()
df_state['PctObeseState'] = df_state['OBESE10']/df_state['2010 Census Population']*100
df_state['PctDiabetesState'] = df_state['DIABETES10']/df_state['2010 Census Population']*100
print 'Top 5 Obese States:'
print df_state.sort_values('PctObeseState', ascending = False)['State Name'][0:5]
print 'Top 5 Diabetic States:'
print df_state.sort_values('PctDiabetesState', ascending = False)['State Name'][0:5]
print '\nLeast 5 Obese States:'
print df_state.sort_values('PctObeseState')['State Name'][0:5]
print '\nLeast 5 Diabetic States:'
print df_state.sort_values('PctDiabetesState')['State Name'][0:5]




#Plot bubble chart of obesity vs diabetes where population represents the bubble size
labels = df_state['State']
colors = np.random.rand(len(df_state))
fig = plt.figure(facecolor='white')
plt.scatter(df_state['PctObeseState'], df_state['PctDiabetesState'],s=df_state['2010 Census Population']*1000/max(df_state['2010 Census Population']),c=colors,alpha=0.5)
for label, x, y in zip(labels, df_state['PctObeseState'], df_state['PctDiabetesState']):
    plt.annotate(
        label, 
        xy = (x, y), xytext = (-20, 20),
        textcoords = 'offset points', ha = 'right', va = 'bottom',
        bbox = dict(boxstyle = 'round,pad=0.25', fc = 'yellow', alpha = 0.5),
        arrowprops = dict(arrowstyle = '->', connectionstyle = 'arc3,rad=0'))
plt.ylabel('Diabetes Percentage',fontsize=25)
plt.xlabel('Obesity Percentage',fontsize=25)
plt.title('Diabetes vs Obesity for all states',fontsize=30)
plt.tick_params(axis='both', labelsize=20)
plt.show()

#Create a column for metro/non metro counties
df_main['Metro_name']=df_main['METRO13'].apply(lambda row: metro_name(row))
#t-test to find if there is a difference in obesity
t, p = stats.ttest_ind(df_main[df_main['METRO13']==0]['PCT_OBESE_ADULTS10'].values.astype(int), df_main[df_main['METRO13']==1]['PCT_OBESE_ADULTS10'].values.astype(int), equal_var=False)
print "For the two sample t-test, t-statistic is {:.3f} and the p-value is {:.3f}" .format(t, p)
#Box plot of obesity for metro and non-metro counties
figure(2)
ax=sns.boxplot(x="Metro_name", y="PCT_OBESE_ADULTS10",  data=df_main)
ax.set_xlabel(None,fontsize=0)
ax.set_ylabel("Obesity Percentage",fontsize=25)
ax.set_title("Obesity across metro/non-metro counties",fontsize=30)
ax.tick_params(axis='x', labelsize=25)
ax.tick_params(axis='y', labelsize=20)
plt.show()

#The columns for which analysis is being done are stored in a variable
cols=['FFRPTH10','FSRPTH10','RECFACPTH10','PCT_NHWHITE10','PCT_NHBLACK10','PCT_HISP10', \
'PCT_NHASIAN10','PCT_65OLDER10','PCT_18YOUNGER10','POVRATE10','MEDHHINC10','SNAPSPTH10','GROCPTH10','SUPERPTHC10','CONVSPTH10','SPECSPTH10','WICSPTH10']

#Find the restaurants and recreation facilites for metro and non-metro counties
df_metro=df_main.groupby(['METRO13']).mean()
df_metro['FFRPTH10']=df_metro['FastFood10']/df_metro['2010 Census Population']
df_metro['FSRPTH10']=df_metro['FullFood10']/df_metro['2010 Census Population']
df_metro['RECFACPTH10']=df_metro['Recfac10']/df_metro['2010 Census Population']
df_metro=df_main.groupby(['METRO13']).mean()

#Drop the categorical variables in the end
df_main = df_main.drop('State Name', 1)
df_main = df_main.drop('Abbrev', 1)
df_main = df_main.drop('Metro_name', 1)

cols=['POVRATE10','MEDHHINC10','PCT_NHWHITE10','PCT_NHBLACK10','PCT_HISP10', \
'PCT_NHASIAN10','PCT_65OLDER10','PCT_18YOUNGER10','METRO13','FFRPTH10','FSRPTH10','RECFACPTH10','SNAPSPTH10','GROCPTH10','SUPERPTHC10','CONVSPTH10','SPECSPTH10','WICSPTH10']
#Find the correlation of columns with obesity percentage
df_corr=df_main[cols].apply(lambda x: x.corr(df_main['PCT_OBESE_ADULTS10']))
df_corr
#Run a multiple linear regression
cols1='FFRPTH10+FSRPTH10+RECFACPTH10+PCT_NHWHITE10+PCT_NHBLACK10+PCT_HISP10+ \
PCT_NHASIAN10+PCT_65OLDER10+PCT_18YOUNGER10+POVRATE10+MEDHHINC10+METRO13+SNAPSPTH10+GROCPTH10+SUPERPTHC10+CONVSPTH10+SPECSPTH10+WICSPTH10'
result = sm.ols(formula='PCT_OBESE10 ~ ' + cols1, data=df_main).fit()
print result.summary()
#Run the regression again after removing the insignificant variables
cols1='FSRPTH10+RECFACPTH10+PCT_NHWHITE10+PCT_NHBLACK10+PCT_HISP10+ \
PCT_NHASIAN10+MEDHHINC10+METRO13+SNAPSPTH10+GROCPTH10+SUPERPTHC10'
result = sm.ols(formula='PCT_OBESE10 ~ ' + cols1, data=df_main).fit()
print result.summary()
figure=smg.graphics.plot_ccpr(result,'FSRPTH10')
plt.ylabel('Obesity percentage',fontsize=25)
plt.xlabel('Full Service restaurants per 1000 population',fontsize=25)
plt.title('Observed vs Fitted values of Regression',fontsize=30)
plt.tick_params(axis='both', labelsize=20)
figure.show()

#Predict missing obese values using regression model
df_missing=df_base[~df_base.index.isin(df_main.index)]
cols=['FSRPTH10','RECFACPTH10','PCT_NHWHITE10','PCT_NHBLACK10','PCT_HISP10', \
'PCT_NHASIAN10','MEDHHINC10','METRO13','SNAPSPTH10','GROCPTH10','SUPERPTHC10']
df_pred=df_missing[cols]
df_pred=df_pred.dropna(how='any')
df_pred['PCT_OBESE10']=result.predict(df_pred)

#Join these missing values to original dataset
cols=['PCT_OBESE10','FSRPTH10','RECFACPTH10','PCT_NHWHITE10','PCT_NHBLACK10','PCT_HISP10', \
'PCT_NHASIAN10','MEDHHINC10','METRO13','SNAPSPTH10','GROCPTH10','SUPERPTHC10']
df_append=pd.concat([df_main[cols],df_pred[cols]])

#Normalize the variables for clustering
df_final=df_append[cols]
col_num=df_final.columns.values
df_norm=df_final.copy()
df_norm[col_num] = df_norm[col_num].apply(lambda x: (x - x.mean()) / (x.std()))
#K-Means clustering
model = KMeans(n_clusters=4)
model.fit(df_norm)
print model.fit
#Principal component analysis to plot cluster in two dimensions
pca_2=PCA(2)
plt.figure(4)
plot_columns=pca_2.fit_transform(df_norm)
plt.scatter(x=plot_columns[:,0],y=plot_columns[:,1],c=model.labels_)
plt.title('K-Means Clusters based on Obesity and significant factors',fontsize=30)
plt.tick_params(axis='both', labelsize=0)
plt.show()

#Append the cluster numbers to original data
df_append['cluster_num']=model.labels_
df_append=DataFrame(df_append['cluster_num'],columns=['cluster_num'])
df_clus = pd.merge(df_base, df_append, left_index=True, right_index=True, how='inner');

#Create a dataframe with sorted obesity means
mean_obesity=df_clus.groupby(['cluster_num'])['PCT_OBESE10'].mean().sort_values()
#Modify the cluster number to be in line with obesity sorting
clus_dict=dict(zip(mean_obesity.index,[x for x in range(0,len(mean_obesity.index))]))
df_clus['cluster']=df_clus['cluster_num'].apply(lambda row: clus_dict[row])
#Create a table with mean of all variables for four clusters
df_profile=df_clus.groupby('cluster').mean()[cols].stack().unstack(0)
print df_profile

#Find all the counties in the US
with open('us_counties.topo.json') as json_data:
    d = json.load(json_data)
county_list=[]
for i in range(0,len(d['objects']['us_counties.geo']['geometries'])):
    county_list.append(d['objects']['us_counties.geo']['geometries'][i]['properties']['FIPS'])
county_list=Series(county_list).unique()
county_list=DataFrame(county_list,columns=["FIPS"])
#Merge the cluster dataset to these counties and create a cluster 0 for missing counties
df_counties=pd.merge(county_list, df_clus, left_on="FIPS", right_on="FIPS",how='left')
df_counties['cluster']=df_counties['cluster'].fillna(-1)
df_counties['cluster']=df_counties['cluster']+1

#Export the dataset so that heat map can be generated in iPython and store it in Notebooks for the Ipython to run smoothly
#os.chdir('C:\Users\<Username>\Documents\Notebooks')
df_output= df_counties[['FIPS','cluster','State']]
df_output=df_output.sort_values(by='FIPS')
df_output.to_csv('Cluster_counties.csv', sep=',')

#Find how percentage of count of counties in each cluster
cluster_pct=((df_clus.groupby('cluster').size()/df_clus.groupby('cluster').size().sum())*100)
cluster_pct=cluster_pct.round(0).astype(int)
#Plot a Donut chart
plt.figure(5)
labels = 'Fit Hispanic', 'Urban Healthy', 'Calorific', 'Afro Obese'
explode=(0, 0, 0, 0.05)
plt.pie(cluster_pct, explode=explode, labels=labels,
                autopct='%1.1f%%', shadow=True, startangle=90,colors=['green', 'lightgreen', 'blue', 'red'])
#title('Cluster distribution', bbox={'facecolor':'0.8', 'pad':5},fontsize=0)
centre_circle = plt.Circle((0,0),0.5,color='black', fc='white',linewidth=1.25)
fig = plt.gcf()
fig.gca().add_artist(centre_circle)
plt.axis('equal')
plt.show()  








