import numpy as np
import pandas as pd

import pickle

from tcsscraper.api.helper import Car



def load_pickle(path):
  with open(path, "rb") as fp:
    pickled = pickle.load(fp)
  
  return pickled
