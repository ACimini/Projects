import numpy as np
import scipy

class GARCHOneOne(object):

    def __init__(self, stock):
        self.stock = stock.dropna()
        self.logReturns = self.log_scaling(self.stock) * 100  # Returns in percent
        self.coefficients = self.garch_optimization()
        self.sigma_2 = self.garch_filter(self.coefficients[:3])  # Only use omega, alpha, beta

    def log_scaling(self, stock):
        """Computes log returns using vectorized operations"""
        log_returns = np.log(stock) - np.log(stock.shift(1))
        return log_returns.dropna().to_numpy()

    def garch_filter(self, params):
        """Returns conditional variances from GARCH(1,1) recursion"""
        omega, alpha, beta = params
        length = len(self.logReturns)
        sigma_2 = np.zeros(length)

        for i in range(length):
            if i == 0:
                sigma_2[i] = omega / (1 - alpha - beta)  # Long-run unconditional variance
            else:
                sigma_2[i] = omega + alpha * self.logReturns[i - 1]**2 + beta * sigma_2[i - 1]

        return sigma_2

    def garch_loglikelihood(self, params):
        """Negative log-likelihood function for GARCH(1,1)"""
        sigma_2 = self.garch_filter(params)
        log_likelihood = np.sum(np.log(sigma_2) + self.logReturns**2 / sigma_2)
        return log_likelihood  # Minimize this (no extra negative needed)

    def garch_optimization(self):
        """Fits the model via MLE and returns coefficients"""
        initial_params = [0.1, 0.05, 0.92]
        bounds = ((0.001, 1), (0.001, 1), (0.001, 1))

        result = scipy.optimize.minimize(self.garch_loglikelihood, initial_params, bounds=bounds)

        if not result.success:
            raise ValueError("Optimization failed: " + result.message)

        omega, alpha, beta = result.x
        long_run_var = 0.01**2 * omega / (1 - alpha - beta)
        return np.append(result.x, long_run_var)
