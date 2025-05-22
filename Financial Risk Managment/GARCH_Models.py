import math
from unittest import skipIf

import numpy as np
import pandas as pd
import scipy
import matplotlib.pyplot as plt

class GARCHOneOne(object):

    def __init__(self,stock):
        self.stock = stock
        self.logReturns = self.log_scaling(stock) * 100
        self.coefficients = self.garch_optimization()
        self.sigma_2 = self.garch_filter(self.coefficients[:3])

    # Look into the math here after done
    def log_scaling(self, stock):
        """Log scales the stock prices and returns the Log returns"""

        log_return = [math.nan] * len(stock)

        for i in range(len(stock)):
            if i == 0:
                log_return[i] = math.nan
            else:
                log_return[i] = math.log(stock[i]) - math.log(stock[i - 1])

        return np.array(log_return)


    def garch_filter(self, params):
        """Returns the variance expression of the GARCH(1,1) model"""

        #Slicing the parameters list for the needed values
        omega = params[0]
        alpha = params[1]
        beta = params[2]

        #Length of log returns
        length = len(self.stock)

        #Empty array of zeros
        sigma_2 = np.zeros(length)

        #Fill the array; if i == 0 then we use the long term variance
        for i in range(length):
            if i == 0:
                sigma_2[i] = omega / (1 - alpha - beta)
            else:
                sigma_2[i] = omega + alpha * self.logReturns[i-1]**2 + beta * sigma_2[i-1]

        return sigma_2


    def garch_loglikelihood(self, params):
        """Defines the log likelihood sum to be optimized given the parameters"""

        length = len(self.logReturns)

        sigma_2 = self.garch_filter(params)

        log_likelihood = - np.sum(-np.log(sigma_2) - self.logReturns**2 / sigma_2)

        return log_likelihood


    def garch_optimization(self):
        """Optimizes the log likelihood function and returns estimated coefficients"""

        #Initializes the parameters
        params =  [0.1, 0.05, 0.92]

        #Parameters optimization, scipy does not have a maximize function, so we minimize the opposite of the equation described earlier
        opt = scipy.optimize.minimize(self.garch_loglikelihood, params, bounds= ((0.001,1),(0.001,1),(0.001,1)))

        variance = 0.01**2 * opt.x[0] / (1 - opt.x[1] - opt.x[2])

        return np.append(opt.x, variance)







