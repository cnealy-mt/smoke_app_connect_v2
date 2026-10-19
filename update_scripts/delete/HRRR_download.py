import importlib

required_packages = ["s3fs", "xarray", "metpy", "boto3", "numcodecs", "numpy", "dataclasses", "datetime", "zarr", "json"]
missing_packages = []

for pkg in required_packages:
    try:
        importlib.import_module(pkg)
    except ModuleNotFoundError:
        missing_packages.append(pkg)

if missing_packages:
    print("Missing packages:", missing_packages)
    print("You can install them using:")
    print(f"pip install {' '.join(missing_packages)}")

#-------------------------These first 2 commented out sections are defined in the R script that sources this Python code
#-------------------------because ZarrId needs to be defined as a class first, and then ZarrId is dynamically defined in R code (with reticulate)
# import dataclasses
# import datetime
# 
# @dataclasses.dataclass
# class ZarrId:
#     run_hour: datetime.datetime
#     level_type: str
#     var_level: str
#     var_name: str
#     model_type: str
#         
#     def format_chunk_id(self, chunk_id):
#         if self.model_type == "fcst": 
#             # Extra id part since forecasts have an additional (time) dimension
#             return "0." + str(chunk_id)
#         else:
#             return chunk_id
#----------------------------------Zarr Variable Template (datetime is a placeholder and is changed later)          
# import datetime
# 
# # Get the date from R (passed via reticulate)
# run_hour = datetime.datetime(year, month, day, 12)  # Using the year, month, and day passed from R
# 
# zarr_id = ZarrId(
#                 run_hour=run_hour, #update this to be run daily
#                 level_type="sfc",
#                 var_level="8m_above_ground",
#                 var_name="MASSDEN",
#                 model_type="fcst"
#                 )
#----------------------------------Create URL Functions          
def create_s3_group_url(zarr_id, prefix=True):
    url = "s3://hrrrzarr/" if prefix else "" # Skip when using boto3
    url += zarr_id.run_hour.strftime(
        f"{zarr_id.level_type}/%Y%m%d/%Y%m%d_%Hz_{zarr_id.model_type}.zarr/")
    url += f"{zarr_id.var_level}/{zarr_id.var_name}"
    return url
  
def create_s3_subgroup_url(zarr_id, prefix=True):
    url = create_s3_group_url(zarr_id, prefix)
    url += f"/{zarr_id.var_level}"
    return url
  
def create_s3_chunk_url(zarr_id, chunk_id, prefix=False):
    url = create_s3_subgroup_url(zarr_id, prefix)
    url += f"/{zarr_id.var_name}/{zarr_id.format_chunk_id(chunk_id)}"
    return url
  
#----------------------------------Define fs
# ATTENTION (5/7/2025): differs from Utah.edu documentation because Zarr v3 dropped support of FSMap file store type
# Must use fsspec directly (don't even know what that means...)
import metpy
import s3fs
import xarray as xr
from zarr.storage._fsspec import FsspecStore  # Import Zarr's fsspec integration

fs = s3fs.S3FileSystem(anon=True)

# Correct way to use FsspecStore
store = FsspecStore.from_url("s3://hrrrzarr/grid/HRRR_chunk_index.zarr", storage_options={"anon": True})

chunk_index = xr.open_zarr(store, consolidated=True)

print(chunk_index)

#----------------------------------Setup boto3          
import boto3
from botocore import UNSIGNED
from botocore.config import Config

# Don't recreate this resource in a loop! That caused a 3-4x slowdown for me.
s3 = boto3.resource(service_name='s3', region_name='us-west-1', config=Config(signature_version=UNSIGNED))

def retrieve_object(s3, s3_url):
    obj = s3.Object('hrrrzarr', s3_url)
    return obj.get()['Body'].read()
#----------------------------------Dtype Function
import json

def get_dtype_from_zmetadata(s3, zarr_id):
    import json

    date_str = zarr_id.run_hour.strftime("%Y%m%d")
    zmeta_key = f"{zarr_id.level_type}/{date_str}/{date_str}_12z_{zarr_id.model_type}.zarr/.zmetadata"

    try:
        zmeta_obj = s3.Object("hrrrzarr", zmeta_key)
        zmeta_raw = zmeta_obj.get()["Body"].read().decode("utf-8")
        zmeta_json = json.loads(zmeta_raw)

        metadata = zmeta_json.get("metadata", {})
        for key in metadata.keys():
          # Indent all code that should run inside the loop by 4 spaces (or a tab)
          var_key = f"{zarr_id.var_level}/{zarr_id.var_name}/{zarr_id.var_level}/{zarr_id.var_name}/.zarray"
          #print(f"Looking for dtype under key: '{var_key}'")

        if var_key in metadata:
            dtype = metadata[var_key].get("dtype")
            if dtype:
                print(f"Found dtype: {dtype}")
                return dtype
            else:
                print(f"Warning: dtype key missing inside {var_key}")
                return None
        else:
            print(f"Warning: {var_key} not found in .zmetadata")
            return None

    except Exception as e:
        print(f"Error accessing metadata for {zarr_id.var_name}: {e}")
        return None


dtype = get_dtype_from_zmetadata(s3, zarr_id)
print(f"Dtype for {zarr_id.var_name}:", dtype)

#----------------------------------Decompression Function          
                
import numcodecs as ncd
import numpy as np

def decompress_chunk(zarr_id, compressed_data, dtype):
    buffer = ncd.blosc.decompress(compressed_data)
    
    # dtype = "<f4" # Note: The default datatype changed to <f4 on 2024-06-01_00z
    #               # When in doubt please check the .zmetadata file for datatypes
    # if zarr_id.var_level == "8m_above_ground" and zarr_id.var_name == "MASSDEN":
    #     dtype = "<f8"
        
    chunk = np.frombuffer(buffer, dtype=dtype)
    
    if zarr_id.model_type == "anl":
        data_array = np.reshape(chunk, (150, 150))
    else:
        entry_size = 22500
        data_array = np.reshape(chunk, (len(chunk)//entry_size, 150, 150))
        
    return data_array

#----------------------------------Get Data          
lat_top = 49.1
lat_bottom = 44.3
lon_top = -103.9    # Eastern longitude (less negative)
lon_bottom = -116.1 # Western longitude (more negative) 

def check_boundaries(data):
    return ((lat_bottom < data.latitude) & (data.latitude < lat_top) & (
        lon_bottom < data.longitude) & (data.longitude < lon_top)).compute()

area = chunk_index.where(check_boundaries, drop=True)
area

def get_unique(data):
    # We have to implement our own "unique" logic since missing values are NaN (a float) and the rest are string
    data = data.fillna(None).values.flatten()
    data = data[data != None]
    return np.unique(data)

chunk_ids = get_unique(area.chunk_id) # NEEDS TO BE RECTANGULAR (i.e, 4 or 6 or etc. chunks...not 3 or 5; adjust lat lon if not rectangular)

def get_chunk(zarr_id, chunk_id, dtype):
    # retrieve data as before
    compressed_data = retrieve_object(s3, create_s3_chunk_url(zarr_id, chunk_id))
    chunk_data = decompress_chunk(zarr_id, compressed_data, dtype)
    
    # combine retrieved data with the chunk grid
    chunk_index.load()
    chunk_xarray = chunk_index.where(lambda x: x.chunk_id == chunk_id, drop=True)
    dimensions = ("y", "x") if zarr_id.model_type == "anl" else ("time", "y", "x")
    chunk_xarray[zarr_id.var_name] = (dimensions, chunk_data)
    return chunk_xarray

def get_chunks_combined(zarr_id, chunk_ids, dtype):
    chunks = [get_chunk(zarr_id, chunk_id, dtype) for chunk_id in chunk_ids]
    return xr.merge(chunks)

    
data = get_chunks_combined(zarr_id, chunk_ids, dtype)
data

#----------------------------------Get Data Timeseries (best for analysis data, change in Zarr id to "anl" if using)         
# import matplotlib.pyplot as plt
# 
# plt.close('all')            # Clear all previous plots
# 
# data.MASSDEN.plot()             # Make the new plot
# plt.show()     
# 
# start = datetime.datetime(2024, 9, 9, 0)
# times = [start + datetime.timedelta(weeks=week_delta) for week_delta in range(2)]
# 
# zarr_ids = [dataclasses.replace(zarr_id, run_hour=time) for time in times]
# 
# def get_data(zarr_ids, chunk_ids, is_forecast):
#     datasets = []
#     for zarr_id in zarr_ids:
#         data = get_chunks_combined(zarr_id, chunk_ids)
#         new_time_dimension = "run_time" if is_forecast else "time"
#         data[new_time_dimension] = zarr_id.run_hour
#         datasets.append(data)
#     ds = xr.concat(datasets, dim=new_time_dimension, combine_attrs="override")
#     return ds
#     
# data = get_data(zarr_ids, chunk_ids, False)
# data


#----------------------------------Pass Data to R for Mapping
# Send 3D array (time, y, x) and lat/lon values
variable_values = data[zarr_id.var_name].values  # shape: (time, y, x)
lat_values = data["latitude"].values[0]  # 1D along y
lon_values = data["longitude"].values[0]  # 1D along x
x_values = data["x"].values  # 1D, meters in Lambert Conformal
y_values = data["y"].values  # 1D, meters in Lambert Conformal
time_values = data["time"].values


# Assign to py object so R can access it
py_variable = variable_values
py_lat = lat_values
py_lon = lon_values
py_x = x_values
py_y = y_values
py_time = time_values
