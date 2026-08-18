import xarray as xr
import numpy as np
import os
import time
import re


def download_geos_variable(
    var_name,
    url_template,
    subset_indices,
    update_str_GEOS,
    run_hour_GEOS,
    out_dir="data/temp",
    max_retries=5,
    wait_initial=5,
):
    """
    Download a GEOS variable via OPeNDAP with retry + zero-value detection.
    """

    base_url = url_template.format(
        update_str_GEOS=update_str_GEOS,
        run_hour_GEOS=run_hour_GEOS,
    )
    
    # Extract the lat and lon slices from your subset string
    # subset_indices is "[time][lat][lon]"
    slices = re.findall(r'\[.*?\]', subset_indices)
    lat_slice = slices[1]  # This gets [516:1:574]
    lon_slice = slices[2]  # This gets [177:1:245]

    # New URL: Ask for the variable AND the lat/lon coordinate arrays
    url = f"{base_url}?{var_name}{subset_indices},lat{lat_slice},lon{lon_slice}"

    print(f"\n📡 GEOS OPeNDAP request for {var_name} with coordinates")
    print(url)

    os.makedirs(out_dir, exist_ok=True)
    out_file = os.path.join(out_dir, f"{var_name}_GEOS_{update_str_GEOS}.nc")

    # ------------------------
    # Retry loop
    # ------------------------
    for attempt in range(max_retries):
        try:
            print(f"Attempt {attempt + 1}/{max_retries}: opening dataset")
            # xarray now sees the variable AND the lat/lon arrays
            ds = xr.open_dataset(url, decode_times=False)

            if var_name not in ds.data_vars:
                raise KeyError(f"{var_name} not found.")

            # --- CHANGE 1: Load the whole dataset to include coordinates ---
            print(f"Downloading {var_name} and coordinate arrays...")
            ds.load()  # This pulls the variable, lat, and lon
            
            # Use the loaded dataset for the zero-check
            if np.all(ds[var_name].values == 0):
                raise RuntimeError("Downloaded data are all zeros")

            # --- CHANGE 2: Fix time encoding on the dataset object ---
            if "time" in ds.coords:
                ds["time"].encoding.pop("units", None)
                ds["time"].attrs.pop("units", None)

            print(f"Saving validated data → {out_file}")
            # Save the whole dataset (ds) so lat/lon are included in the file
            ds.to_netcdf(out_file, format="NETCDF4")
            
            ds.close()
            print(f"✅ {var_name} download complete (with dimvars)")
            return out_file

        except Exception as e:
            print(f"⚠️ Attempt {attempt + 1} failed: {e}")

            # Make absolutely sure the dataset is closed
            try:
                ds.close()
            except Exception:
                pass

            if attempt < max_retries - 1:
                wait = wait_initial * (2 ** attempt)
                print(f"Retrying in {wait} seconds...")
                time.sleep(wait)
            else:
                raise RuntimeError(
                    f"❌ {var_name} download failed after {max_retries} attempts"
                )

def main():
    global update_str_GEOS, run_hour_GEOS

    geos_vars = [
        {
            "var": "pm25",
            "url": "https://opendap.nccs.nasa.gov/dods/GEOS-5/fp/0.25_deg/fcast/"
                   "tavg3_2d_aer_Nx/tavg3_2d_aer_Nx.{update_str_GEOS}_{run_hour_GEOS}",
                   "subset": "[0:1:79][516:1:574][177:1:245]" # 3-hourly time steps
                          #time-steps   #lat       #lon       # see dds and das info at opendap server url for info on lat/lon structure & resolution; the lat/lon slices here represent the defined bounding box for this app 
        },
        {
            "var": "pblh",
            "url": "https://opendap.nccs.nasa.gov/dods/GEOS-5/fp/0.25_deg/fcast/"
                   "tavg1_2d_flx_Nx/tavg1_2d_flx_Nx.{update_str_GEOS}_{run_hour_GEOS}",
                   "subset": "[0:1:239][516:1:574][177:1:245]" # Hourly time steps
        },
        {
            "var": "v10m",
            "url": "https://opendap.nccs.nasa.gov/dods/GEOS-5/fp/0.25_deg/fcast/"
                   "tavg1_2d_slv_Nx/tavg1_2d_slv_Nx.{update_str_GEOS}_{run_hour_GEOS}",
                   "subset": "[0:1:239][516:1:574][177:1:245]" # Hourly time steps
        },
        {
            "var": "u10m",
            "url": "https://opendap.nccs.nasa.gov/dods/GEOS-5/fp/0.25_deg/fcast/"
                   "tavg1_2d_slv_Nx/tavg1_2d_slv_Nx.{update_str_GEOS}_{run_hour_GEOS}",
                   "subset": "[0:1:239][516:1:574][177:1:245]" # Hourly time steps
        },
    ]

    # Added enumerate here so 'i' works
    for i, item in enumerate(geos_vars):
        download_geos_variable(
            var_name=item["var"],
            url_template=item["url"],
            subset_indices=item["subset"],
            update_str_GEOS=update_str_GEOS,
            run_hour_GEOS=run_hour_GEOS,
        )

        # Cooldown logic
        if i < len(geos_vars) - 1: 
            print("\n⏳ Cooldown: Waiting 15s to let NCCS server buffers clear...")
            time.sleep(15)



if __name__ == "__main__":
    main()
