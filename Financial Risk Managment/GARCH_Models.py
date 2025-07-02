import numpy as np
import pandas as pd
import scipy
from scipy.stats import norm
from scipy.stats import t
from scipy.stats import binomtest

class GARCHOneOne(object):
    def __init__(self, stock):
        self.stock = stock.dropna()
        self.logReturns = self.log_scaling(self.stock) * 100  # Working with percent returns
        self.coefficients = self.garch_optimization()
        self.sigma_2 = self.garch_filter(self.coefficients)

    def log_scaling(self, stock):
        """Computes log returns using vectorized operations"""
        log_returns = np.log(stock) - np.log(stock.shift(1))
        log_returns = log_returns[1:]
        return log_returns.dropna().to_numpy()

    def garch_filter(self, params):
        """Returns conditional variances from GARCH(1,1) recursion"""

        omega = params[0]
        alpha = params[1]
        beta = params[2]

        length = len(self.logReturns)

        sigma_2 = np.zeros(length)

        for i in range(length):
            if i == 0:
                sigma_2[i] = omega / (1 - alpha - beta)
            else:
                sigma_2[i] = omega + alpha * self.logReturns[i - 1]**2 + beta * sigma_2[i - 1]

        return sigma_2

    def garch_loglikelihood(self, params):
        """Negative log-likelihood function for GARCH(1,1)"""
        sigma_2 = self.garch_filter(params)
        log_likelihood = - np.sum(-np.log(sigma_2) - self.logReturns**2 / sigma_2)
        return log_likelihood  # Minimize this

    def garch_optimization(self):
        """Fits the model via MLE and returns coefficients"""
        initial_params = [0.01, 0.05, 0.9]
        bounds = ((.001,1),(.001,1),(.001,1))

        opt = scipy.optimize.minimize(self.garch_loglikelihood, initial_params, bounds=bounds)
        variance = 0.01**2 * opt.x[0] / (1 - opt.x[1] - opt.x[2])

        return np.append(opt.x, variance)

    def garch_var_norm(self,confidence_level,time_horizon,):
        z_score = norm.ppf(1 - confidence_level)
        latest_sigma = np.sqrt(self.sigma_2[-1])

        var = - z_score * latest_sigma * np.sqrt(time_horizon)

        return var

    def garch_var_t(self,confidence_level,time_horizon,):
        df, loc, scale = t.fit(self.logReturns)
        t_quantile = t.ppf(1 - confidence_level,df)

        last_sigma = np.sqrt(self.sigma_2[-1])

        var = - t_quantile * last_sigma * np.sqrt(time_horizon)
        return var



class RollingVaRBacktest(object):
    def __init__(self, stock, confidence_interval, window_size, time_horizon, GARCH_Class, method):
        self.stock = stock.dropna()
        self.time_horizon = time_horizon
        self.method = method
        self.confidence_interval = confidence_interval
        self.window_size = window_size
        self.GARCH_Class = GARCH_Class

    def var_backtest(self):
        violations = []
        var_series = []

        for i in range(self.window_size, len(self.stock) - 1):
            window_data = self.stock.iloc[i - self.window_size:i + 1]
            model = self.GARCH_Class(window_data)

            if self.method == "t":
                var = model.garch_var_norm(confidence_level=self.confidence_interval, time_horizon=self.time_horizon)
            elif self.method == "n":
                var = model.garch_var_t(confidence_level=self.confidence_interval, time_horizon=self.time_horizon)
            else:
                raise Exception("Method must be 't' or 'n'")

            actual_return = model.logReturns[-1]  # most recent return in the current window
            violations.append(actual_return < -var)
            var_series.append(var)

        violations = np.array(violations)
        var_series = np.array(var_series)

        hit_rate = np.mean(violations)
        expected_hit_rate = 1 - self.confidence_interval

        num_violations = int(np.sum(violations))
        test_result = binomtest(num_violations, len(violations), (1 - self.confidence_interval), alternative='two-sided')

        print(f"Backtest Results ({self.method}):")
        print(f"  Violations: {np.sum(violations)} out of {len(violations)}")
        print(f"  Hit Rate: {hit_rate:.4f} (Expected: {expected_hit_rate:.4f})")
        print(f"  Test p-value: {test_result.pvalue:.4f}")

        return pd.Series(var_series), pd.Series(violations.flatten())

