# -*- coding: utf-8 -*-
"""
Created on Fri Dec  9 17:26:20 2016

@author: damien
"""


import pandas as pd
import numpy as np
from datetime import datetime as dt
from datetime import timedelta as td
import time
import pickle
from sklearn.ensemble import RandomForestRegressor as RFR
from sklearn.linear_model import LinearRegression as LR
from sklearn.linear_model import SGDRegressor as SGD
from sklearn.linear_model import RandomizedLasso as RLasso
from sklearn.linear_model import PassiveAggressiveRegressor as PAR
from sklearn.linear_model import ElasticNet as ElasticNet
from sklearn.linear_model import LassoLars as LL
from sklearn.linear_model import BayesianRidge as BRidge
from sklearn.linear_model import Ridge
from sklearn.linear_model import TheilSenRegressor as TSR
from sklearn.linear_model import orthogonal_mp as OMP
from sklearn.neural_network import MLPRegressor
from sklearn.linear_model import Lasso
from sklearn.ensemble import AdaBoostRegressor as ABR
from sklearn.ensemble import GradientBoostingRegressor as GBR
from sklearn.svm import SVR
    
from sklearn.feature_selection import SelectFromModel    
from sklearn.linear_model import LassoLars
import math
from sklearn.ensemble import AdaBoostRegressor as ABR
from sklearn.ensemble import BaggingRegressor as BR
from sklearn.ensemble import ExtraTreesRegressor as ETR
from sklearn.pipeline import make_pipeline
from sklearn.decomposition import PCA, KernelPCA
from sklearn.decomposition import TruncatedSVD
from sklearn.preprocessing import StandardScaler
from sklearn.cross_validation import cross_val_score
from sklearn.model_selection import KFold
from joblib import Parallel,delayed
from sklearn.preprocessing import PolynomialFeatures

from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import LabelBinarizer

def findWeather(date,roweather):
    dat=dt.strptime(date,'%Y-%m-%d %H:%M:%S')
    toc=time.mktime(dat.timetuple())
    start=dt.strftime(-td(minutes=60)+dat,'%Y-%m-%d %H:%M:%S')
    end=dt.strftime(td(minutes=60)+dat,'%Y-%m-%d %H:%M:%S')
    tim=roweather[np.multiply(start<roweather.index,roweather.index<end)]
    tim=[time.mktime(dt.strptime(x,'%Y-%m-%d %H:%M:%S').timetuple())-toc for x in tim.index]    
    if len(tim)==0:
        return [np.nan]*len(roweather.columns)
    #ret=tim[np.argmin(np.absolute(tim))]+toc
    ret=tim[np.argmin(np.absolute(tim))]+toc
    #reto=dt.strftime(dt.fromtimestamp(ret),'%Y-%m-%d %H:%M:%S')
    reto=dt.strftime(dt.fromtimestamp(ret),'%Y-%m-%d %H:%M:%S')
    return list(roweather.loc[reto])


def exportData(row,i):
    station=pd.read_csv(row['city']+'/stations/'+row['station']+'.csv',index_col=0)
    weather=pd.read_csv(row['city']+'/weather.csv',index_col=0)

    times=[dt.strptime(x,'%Y-%m-%d %H:%M:%S') for x in weather.index]
    
    
    tmp=[findWeather(x,weather) for x in station.index]
    #clo=[[tmp[i][j] for i in range(len(tmp))] for j in range(len(tmp[0]))]
    
    neweather=pd.DataFrame(tmp,columns=weather.columns)
    
    station.index=range(len(station))
    data=pd.concat([neweather,station],axis=1).drop(labels=['spaces'],axis=1)
    data.to_csv(row['station'] + '_data')

def selectInfoDate(date):
    x=dt.strptime(date,'%Y-%m-%d %H:%M:%S')
    y=dt(x.year,x.month,x.day)
    return int(x.weekday()),int((x-y).seconds/60)

def appendTime(row,i):
    station=pd.read_csv(row['city']+'/stations/'+row['station']+'.csv')
    weather=pd.read_csv(row['station'] + '_data',index_col=0)
    
    dates=pd.DataFrame([selectInfoDate(station.loc[i,'moment']) for i in range(len(station))],columns=['day','minutes'])  
    
    datess=station['moment']
    station=pd.concat([dates,station['bikes']],axis=1)    
    
    cols=station.columns
    colx=cols[:-1]
    coly=cols[-1]
    
    colxi=colx
    i=colxi[0]
    for i in colxi:
        if station[i].dtype=='O':
            number=LabelEncoder()
            station[i]=number.fit_transform(station[i].astype('str'))
            lb=LabelBinarizer().fit(station[i])
            toreplace=pd.DataFrame(lb.transform(station[i]))
            toreplace.columns=[i+str(j) for j in range(len(toreplace.columns))]
            toreplace.index=range(len(station))
            station=station.drop(labels=[i],axis=1)
            station=pd.concat([station,toreplace.loc[:,toreplace.columns[:len(toreplace.columns)-1]]],axis=1)
    
    res=pd.concat([weather,station.drop(labels='bikes',axis=1),datess],axis=1)
    res.to_csv(row['station'] + '_fulldata')


def binarize(x,k):
    x=int(x)
    ret=[0]*k
    ret[x]=1
    return ret[:k-1]

def binarizeDay(x):
    return binarize(x,7)

def binarizeHour(x):
    return binarize(x,24)
    

def finalizeData(row,i):
    data=pd.read_csv(row['station']+'_fulldata',index_col=0)
    datess=data['moment']
    data=data.drop(labels=['moment'],axis=1)
    data=data.dropna()
    data.index=range(len(data))
    cols=data.columns
    colx=cols[:-1]
    coly=cols[-1]
    
    
    colxi=colx
    i=colxi[1]
    for i in colxi:
        if data[i].dtype=='O':
            number=LabelEncoder()
            data[i]=number.fit_transform(data[i].astype('str'))
            if i!='description':
                lb=LabelBinarizer().fit(data[i])
                toreplace=pd.DataFrame(lb.transform(data[i]))
                toreplace.columns=[i+str(j) for j in range(len(toreplace.columns))]
                toreplace.index=range(len(data))
                data=data.drop(labels=[i],axis=1)
                data=pd.concat([data,toreplace.loc[:,toreplace.columns[:len(toreplace.columns)-1]]],axis=1)
    
    price=data['bikes']
    data=data.drop(labels=['bikes'],axis=1)
    data=pd.concat([data,price],axis=1)
    cols=data.columns
    colx=cols[:-1]
    coly=cols[-1]
    clean=data.dropna()
    scaler = StandardScaler().fit(clean)
    scalert = StandardScaler().fit(clean[cols[:-1]])
    
     
    sc=StandardScaler().fit(data)
    scy=StandardScaler().fit(data[coly])
    
    data.to_csv(row['station']+'_finaldata')
    output = open(row['station']+'_scalert', 'wb')
    pickle.dump(scalert,output)
    output.close()
    output = open(row['station']+'_scy', 'wb')
    pickle.dump(scy,output)
    output.close()



def predict(row,i,roweather):
    print(i)
    data=pd.read_csv(row['station']+'_finaldata',index_col=0)
    output = open(row['station']+'_scy', 'rb')
    scy=pickle.load(output)
    output.close()   
    output = open(row['station']+'_scalert', 'rb')
    scalert=pickle.load(output)
    output.close()   
    output = open(row['station']+'_modelrfr', 'rb')
    m=pickle.load(output)
    output.close()
    #output = open(row['station']+'_list', 'rb')
    #l=pickle.load(output)
    #output.close()
    
    if m==0:
        return np.mean(data['bikes'])

    row
    fh=selectInfoDate(row['Unnamed: 0'])
    
    fh=pd.DataFrame([fh],columns=['day','minutes'])
    
    tes=pd.DataFrame([findWeather(row.loc['Unnamed: 0'],roweather)],columns=roweather.columns)
    tes=pd.concat([tes,fh],axis=1)    
    
    cols=tes.columns
    data2=pd.read_csv(row['station']+'_fulldata',index_col=0).dropna()
    cols2=data2.columns
    cols1=cols2[:-1]
    colx1=cols1[:-1]
    coly1=cols2[-1]
    cols=np.append(colx1,coly1)
    data1=data2[cols]
    colxi1=colx1
    colsi=cols[:-1]
    
    i=colsi[1]
    for i in colsi:
        if data1[i].dtype=='O':
            number=LabelEncoder()
            data1[i]=number.fit_transform(data1[i].astype('str'))
            try :
                tes[i]=number.transform(tes[i].astype('str'))
            except:
                pass
    
    
    colt=tes.columns
        
    
    cols=data.columns
    sct=StandardScaler().fit(data[cols[:-1]])

    ttest=pd.DataFrame(sct.transform(tes),columns=colt)
    #poly=PolynomialFeatures(5)
    #X=poly.fit_transform(ttest)
        
    #pred=scy.inverse_transform(m.predict(X[:,l]))
    pred=scy.inverse_transform(m.predict(ttest))
    return pred[0]



def transformData(row,i):
    print(i)
    exportData(row,i)
    appendTime(row,i)
    finalizeData(row,i)


def prepareModels(row,k):
    data=pd.read_csv(row['station']+'_finaldata',index_col=0)
    output = open(row['station']+'_scy', 'rb')
    scy=pickle.load(output)
    output.close()      
    
    colm1=data.columns
    colx=colm1[:-1]
    data1=data[colm1].dropna()
    coly='bikes'
    sc=StandardScaler().fit(data1)
    data3=data1.iloc[np.random.permutation(len(data1))]
    data3.index=range(len(data3))
    x=pd.DataFrame(sc.transform(data3),columns=colm1)[colx]
    
    y=scy.transform(data3[coly].reshape(-1,1))
    
    
    m=ETR(n_estimators=100,max_depth=30).fit(x,y)
    s=cross_val_score(estimator=m,X=x,y=y,cv=10)
    m.score(x,y)
    np.mean(s)
    
    output = open(row['station']+'_modelrfr', 'wb')
    pickle.dump(m,output)
    output.close()
    err=((1-np.mean(s))*np.var(data1[coly]))**0.5   
    print(k,np.mean(s))

    return k,err,m.score(x,y),np.mean(s)

