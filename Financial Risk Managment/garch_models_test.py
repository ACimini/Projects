import yfinance as yf
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import GARCH_Models

SPX = yf.download('^GSPC', start='2005-01-01', interval='1d')
vix = yf.download('^VIX', start='2005-01-01', interval='1d')

model_SPX = GARCH_Models.GARCHOneOne(SPX['Close'])


# Align index with model's sigma_2 output
dfSPX = pd.DataFrame(np.sqrt(model_SPX.sigma_2 * 252),
                     index=SPX['Close'].dropna().index[1:],
                     columns=['Annualized Volatility'])


plt.plot(dfSPX, label = 'SPX GARCH(1,1)', linewidth = .75)
plt.plot(vix.Close, label = 'VIX', linewidth = .75)
plt.legend()
plt.title('GARCH(1,1) processes and VIX', fontweight = 'bold')
plt.show()