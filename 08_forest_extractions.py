# -*- coding: utf-8 -*-
"""
Created on Mon Oct 28 13:46:01 2019

@author: rr70wedu
"""

#-----------------------------------------------------------------------------------------------#
# 0. load modules and arguments
#-----------------------------------------------------------------------------------------------#

from argparse import ArgumentParser
import rasterio as rt
from rasterio.mask import mask
import glob2 as g
import pandas as pd
import fiona as fn
import numpy as np
import sys
import os

parser = ArgumentParser(description = 'Detailed classification of water bodies')

parser.add_argument("index", help = "file index")

options = parser.parse_args()
index = int(options.index)

#bdir = '/work/remelgad/tenureTrack/'
bdir = '/data/idiv_meyer/01_projects/Andrea/P1/early2020jobs/yearlyChange/'
idir = bdir + '/input/'
odir = bdir + '/output/'

file = g.glob(idir + '*.csv')[index]
tmp = os.path.basename(file).split('_')

gid = pd.read_csv(file)['gid'].values

sy = tmp[0].split('-')[0] # start year
ey = tmp[0].split('-')[1] # end year
region = tmp[1].split('.')[0].lower() # region

print('region: ' + region + ' (' + sy + '-' + ey + ')')

#-----------------------------------------------------------------------------------------------#
# 1. setup base variables
#-----------------------------------------------------------------------------------------------#

fcc = [1,2,3,4,5,9,10] # forest cover codes
ncc = [11,12,13,32] # natural cover code
acc = [14,15,18,19,20,21] # agricultural cover codes

bdir = '/data/idiv_meyer/00_data/processed/mapBiomas/'

sds = rt.open(bdir + 'mapBiomas-' + region + '_' + sy + '0101_01arcSec.vrt') # 1st image
eds = rt.open(bdir + 'mapBiomas-' + region + '_' + ey + '0101_01arcSec.vrt') # last image

colNames = ['gid', 'f_' + sy, 'f_' + ey, 'a_' + sy, 'a_' + ey, 'f-a_' + sy + '-' + ey, 'a-f_' + sy + '-' + ey]

#-----------------------------------------------------------------------------------------------#
# 2. build processing function
#-----------------------------------------------------------------------------------------------#

def main(f):
    
    gid = f['properties']['gid']
    f = [f['geometry']]
    
    # quantify forest change
    try:
        sy = mask(sds, f, crop=True, all_touched=True, indexes=1)[0] # 1st yeart
        ey = mask(eds, f, crop=True, all_touched=True, indexes=1)[0] # last year
    except:
        fa1 = -1
        fa2 = -1
        ac1 = -1
        ac2 = -1
        fac = -1
        afc = -1
    else:
        fa1 = np.sum(np.isin(sy, fcc)) # start year forest
        fa2 = np.sum(np.isin(ey, fcc)) # end year forest
        ac1 = np.sum(np.isin(sy, acc)) # start year agriculture
        ac2 = np.sum(np.isin(ey, acc)) # end year agriculture
        fac = np.sum(np.isin(sy, fcc) & np.isin(ey,acc)) # forest to agriculture
        afc = np.sum(np.isin(sy, acc) & np.isin(ey,fcc)) # agriculture to forest
        sy = None
        ey = None
    
    df = (gid, fa1, fa2, ac1, ac2, fac, afc) # build table
    
    #print('df size %s' % sys.getsizeof(df))
    os.system('echo ' + ('df size %s' % sys.getsizeof(df)))
    
    return(df)

#-----------------------------------------------------------------------------------------------#
# 3. process regions in parallel
#-----------------------------------------------------------------------------------------------#

sp = fn.open('/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/inputs/00data/shp/' + region + '.shp', 'r') # reference polygons
sid = np.array([s['properties']['gid'] for s in sp])
ind = list(np.where(np.isin(sid, gid))[0])

print('number of polygons: ' + str(len(ind)))

odf = [main(sp[int(i)]) for i in ind]
print('df size %s' % sys.getsizeof(odf))
odf = pd.DataFrame(odf, columns = colNames)
odf.to_csv(odir + 'landTenure_stats-' + region + '_' + sy + '-' + ey + '.csv', index=False) # write table
