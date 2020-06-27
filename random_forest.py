#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jun 27 18:38:00 2020

@author: mimuratakuma
"""

#Random Forest 

# ライブラリの読み込み
from sklearn.ensemble import RandomForestClassifier
from sklearn import datasets
from sklearn.model_selection import train_test_split

# irisデータの読み込み
iris = datasets.load_iris()

# 特徴量とターゲットの取得
data       = iris['data']
target     = iris['target']

#学習データをテストデータを分割
train_data,test_data,train_target,test_target = train_test_split(data,target,test_size=0.5)

#モデル学習
model = RandomForestClassifier(n_estimators=100)
model.fit(train_data, train_target)

# 正解率を表示
model.score(test_data, test_target)

#0.9333333333333333


