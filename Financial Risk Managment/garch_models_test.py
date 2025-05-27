import yfinance as yf
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import GARCH_Models

# Download data
SPX = yf.download('^GSPC', start='2005-01-01')['Close']
CAC = yf.download('^FCHI', start='2005-01-01')['Close']
VIX = yf.download('^VIX', start='2005-01-01')['Close']

# Fit GARCH(1,1) models
model_SPX = GARCH_Models.GARCHOneOne(SPX)
model_CAC = GARCH_Models.GARCHOneOne(CAC)

# Annualized GARCH volatility series
vol_SPX = pd.Series(np.sqrt(model_SPX.sigma_2 * 252), index=SPX.dropna().index[1:])
vol_CAC = pd.Series(np.sqrt(model_CAC.sigma_2 * 252), index=CAC.dropna().index[1:])

# Align all series to the same index
common_index = vol_SPX.index.intersection(vol_CAC.index).intersection(VIX.index)

vol_SPX_aligned = vol_SPX.loc[common_index]
vol_CAC_aligned = vol_CAC.loc[common_index]
vix_aligned = VIX.loc[common_index]

# Plot
plt.plot(vol_SPX_aligned, label='SPX GARCH(1,1)', linewidth=0.4)
plt.plot(vol_CAC_aligned, label='CAC 40 GARCH(1,1)', linewidth=0.4)
plt.plot(vix_aligned, label='VIX (Implied)', alpha=0.2)
plt.title("GARCH(1,1) Volatility: SPX vs. CAC vs. VIX", fontweight='bold')
plt.ylabel("Annualized Volatility (%)")
plt.xlabel("Date")
plt.legend()
plt.show()


#norm_95 = model_SPX.garch_var_norm(confidence_level=0.95,time_horizon=1)
#print(norm_95)
#t_95 = model_SPX.garch_var_t(confidence_level=0.95,time_horizon=1)
#print(t_95)

var_SPX_500 = GARCH_Models.RollingVaRBacktest(SPX, 0.95, 500, 1, GARCH_Models.GARCHOneOne, "t")

var_series , violations = var_SPX_500.var_backtest()
print(var_series)
print(violations)
