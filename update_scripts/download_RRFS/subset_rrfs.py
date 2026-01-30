#1b) access and subset RRFS data from AWS S3
import requests
import xarray as xr
import numpy as np
import os
from collections import defaultdict

# Your lat/lon bounds for subsetting
lat_min, lat_max = 39, 53.5
lon_min, lon_max = -124.75, -103.5

# metadata_df is injected by reticulate
metadata = metadata_df.to_dict(orient="records")

def download_partial_grib(url, start_byte, end_byte, filename):
    headers = {"Range": f"bytes={start_byte}-{end_byte}"}
    response = requests.get(url, headers=headers)
    response.raise_for_status()
    with open(filename, "wb") as f:
        f.write(response.content)

def spatial_subset(ds):
    # Assumes 2D lat and lon coordinates named 'latitude' and 'longitude'
    subset = ds.where(
        (ds.latitude >= lat_min) & (ds.latitude <= lat_max) &
        (ds.longitude >= lon_min) & (ds.longitude <= lon_max),
        drop=True
    )
    return subset


def main():
    
    # Define Folders for temporarily saving grib2 and netCDF files
    temp_dir = os.path.join("data", "temp")
    os.makedirs(temp_dir, exist_ok=True)
    
    base_url_template = (
        "https://noaa-rrfs-pds.s3.amazonaws.com/"
        "rrfs_a/rrfs.{update_str}/{run_hour}/"
        "rrfs.t{run_hour}z.prslev.3km.{forecast_hour}.na.grib2"
    )

    # ----------------------------
    # 1. Group metadata by variable
    # ----------------------------
    metadata_by_var = defaultdict(list)
    for row in metadata:
        metadata_by_var[row["var_name"]].append(row)

    # ----------------------------
    # 2. Process each variable separately
    # ----------------------------
    for var_name, rows in metadata_by_var.items():

        out_file = os.path.join(
            temp_dir,
            f"{var_name.upper()}_subset_stack_{update_str}.nc"
        )

        # ---- Skip if already processed
        if os.path.exists(out_file):
            print(f"\n=== Skipping {var_name}: {out_file} already exists ===")
            continue

        print(f"\n=== Processing variable: {var_name} ===")

        layers = []
        grib_files = []

        for row in rows:
            forecast_hour = row["forecast_hour"]

            grib_url = base_url_template.format(
                update_str=update_str,
                run_hour=run_hour,
                forecast_hour=forecast_hour
            )

            partial_file = os.path.join(
                temp_dir,
                f"partial_{var_name}_{forecast_hour}.grib2"
            )
            grib_files.append(partial_file)

            print(f"  → {var_name} f{forecast_hour}")

            download_partial_grib(
                grib_url,
                int(row["start_byte"]),
                int(row["end_byte"]),
                partial_file
            )

            ds = xr.open_dataset(
                partial_file,
                engine="cfgrib",
                backend_kwargs={"indexpath": ""}
            )

            ds_sub = spatial_subset(ds)

            # attach forecast hour as dimension
            ds_sub = ds_sub.expand_dims(
                {"forecast_hour": [forecast_hour]}
            )

            layers.append(ds_sub)

        # ----------------------------
        # 3. Stack forecast hours
        # ----------------------------
        combined = xr.concat(layers, dim="forecast_hour")

        # --- NEW: Standardize variable name for R ---
        # This ensures R doesn't have to guess which variable to read
        var_to_save = list(combined.data_vars)[0]
        combined = combined.rename({var_to_save: 'data_value'})

        # ---- Add provenance metadata (highly recommended)
        combined.attrs.update({
            "rrfs_update_date": update_str,
            "rrfs_run_hour": run_hour,
            "source": "NOAA RRFS",
            "created_by": "RRFS subset pipeline"
        })

        combined.to_netcdf(out_file)
        print(f"Saved {out_file}")

        # ----------------------------
        # 4. Cleanup GRIB files
        # ----------------------------
        for f in grib_files:
            try:
                os.remove(f)
            except FileNotFoundError:
                pass




if __name__ == "__main__":
    main()


