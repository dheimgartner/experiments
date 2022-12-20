import numpy as np
import pandas as pd

import pickle

from tcsscraper.api.helper import Car



def load_pickle(path):
  with open(path, "rb") as fp:
    pickled = pickle.load(fp)
  
  return pickled



def gen_df_archs():
    archs = np.stack(np.meshgrid(Car.vehicle_classes, Car.fuel_types), axis=-1).reshape(
        -1, 2
    )
    df_archs = pd.DataFrame(archs)
    df_archs.columns = ["vehicle_class", "fuel_type"]

    df_archs["car_objects"] = df_archs.apply(
        lambda row: Car(row["vehicle_class"], row["fuel_type"], fuel_consumption=None),
        axis=1,
    )
    
    return df_archs
