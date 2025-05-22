import math
from unittest import skipIf

import numpy as np
import pandas as pd
import scipy
import matplotlib.pyplot as plt

class GARCHOneOne(object):

    def __init__(self,stock,start,end):
        self.logReturns = self.log_scaling(stock) * 100
        self.stock = stock
        self.start = start
        self.end = end

    # Look into the math here after done
    def log_scaling(self, stock):
        """Log scales the stock prices and returns the Log returns"""

        log_return = [math.nan] * len(stock)

        for i in range(len(stock)):
            if i == 0:
                log_return[0] = math.nan
            else:
                log_return[i] = math.log(stock[i]) - math.log(stock[i - 1])

        return log_return


    def garch_filter(self, params):
        """Returns the variance expression of the GARCH(1,1) model"""

        omega = params['omega']
        alpha = params['alpha']
        beta = params['beta']

        #Length of log returns
        length = len(self.stock)



