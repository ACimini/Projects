import numpy as np

__is_none_value = lambda x: (type(x) is float and np.isnan(x)) or x is None

def all_are_some(values) -> bool:
    for val in values:
        if __is_none_value(val):
            return False
    return True
def none_are_some(values) -> bool:
    for val in values:
        if not __is_none_value(val):
            return False
    return True
