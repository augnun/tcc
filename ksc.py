#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 15 22:31:29 2017

@author: augusto
"""

import pandas as pd
import numpy as np
from scipy.spatial.distance import pdist, squareform

# Carrega dados
dados = pd.read_csv("dados_hex/dados.csv", header = 'infer')

# Pré-alocação
llr_z = [None]*203**2
zonas = [None]*203**2


# Define função
def llrKsc(dados):
    pop_total = pd.DataFrame.sum(dados['Pop'])
    casos_total = pd.DataFrame.sum(dados['Casos'])
    
    mat_dist = pdist(dados.ix[:,[2,3]], 'euclidean')
    mat_dist = squareform(mat_dist)
    mat_dist_ind = np.argsort(mat_dist)


    
    
    for i in range(len(dados.index)):
        k = 1
        zona = i
        n_z = pd.DataFrame.sum(dados.ix[[zona], 4])
        if n_z > pop_total:
            k+=1
            next()
        c_z = pd.DataFrame.sum(dados.ix[[zona], 1])
        mu_z = casos_total * (n_z/pop_total)
        if c_z > mu_z:
            llr_z[k] = c_z * np.log(c_z / mu_z) + (casos_total - c_z) * np.log((casos_total - c_z) /
                                                                 (casos_total - mu_z))
        else:
            llr_z[k] = 0
        zonas[k] = zona
        k += 1
        for j in mat_dist_ind[:,i]:
            zona = [i] + [j]
            n_z = pd.DataFrame.sum(dados.ix[zona, 4])
            if n_z > pop_total:
                k+=1
                next()
            if c_z > mu_z:
                llr_z[k] = c_z * np.log(c_z / mu_z) + (casos_total - c_z) * np.log((casos_total - c_z) /
                                                                 (casos_total - mu_z))
            else:
                llr_z[k] = 0
            zonas[k] = zona
            k += 1
    return pd.DataFrame({'Zonas' : [zonas], 'LLR_z' : [llr_z]})
        
llrKsc(dados)

