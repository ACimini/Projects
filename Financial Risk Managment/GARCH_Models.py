import numpy as np
import scipy

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
