# -*- coding: utf-8 -*-
"""
Created on Sun Dec 11 00:59:51 2016

@author: damien
"""

from functions import *

test=pd.read_csv('test-blank.csv')
weather=pd.read_csv('test-weather.csv',index_col=0)

stations=test.drop_duplicates(['city','station'])
stations.index=range(len(stations))


res=Parallel(n_jobs=2)(delayed(transformData)(stations.loc[i],i) for i in range(len(stations)))


res=Parallel(n_jobs=2)(delayed(prepareModels)(stations.loc[i],i) for i in range(len(stations)))


res=np.array(res)
cols=['nstation','bikeserr','scoretrain','cvscore']
resu=pd.DataFrame(res,columns=cols)
resu.to_csv('models')
np.mean(resu['bikeserr'])
resu.sort_values(by='cvscore')
resu.sort_values(by='bikeserr')

i=16
row=stations.loc[i]
row=test.loc[i]
roweather=weather[weather['city']==test.loc[i,'city']].drop(labels=['city'],axis=1)



res=Parallel(n_jobs=6)(delayed(predict)(test.loc[i],i,weather[weather['city']==test.loc[i,'city']].drop(labels=['city'],axis=1)) for i in range(len(test)))




bikes=pd.DataFrame(res,columns=['bikes'])
test['bikes']=bikes

test.index=test['Unnamed: 0']
test=test.drop(labels=['Unnamed: 0'],axis=1)
test.index.name=''


test.to_csv('predictions.csv')

plt.plot(np.array(test['bikes']))

