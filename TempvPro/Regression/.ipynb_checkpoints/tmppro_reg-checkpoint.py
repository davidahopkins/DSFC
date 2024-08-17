#import necessary libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import LinearRegression

data_path = input("enter the path for the data:")
data = pd.read_csv(data_path)

plt.scatter(x='Temp', y='Production', data=data)
plt.show()

X = pd.DataFrame(data['Temp'])
Y = pd.DataFrame(data['Production'])

pn = PolynomialFeatures(degree=2, include_bias=False)
pnx = pn.fit_transform(X)

reg_model = LinearRegression()
reg_model.fit(pnx, Y)
pro_predict = reg_model.predict(pnx)

print("coefficients:", reg_model.coef_, "intercept:", reg_model.intercept_)

plt.scatter(X, Y)
plt.plot(X, pro_predict)
plt.show()