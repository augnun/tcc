#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr  6 11:31:02 2017

@author: augusto
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


# Carrega dados
dados = pd.read_csv("dados_hex/dados.csv", header = 'infer')

# Matriz de adjacências
mat_adj = pd.read_table("dados_hex/hex.adj", header = None)
vizinhos = pd.DataFrame()