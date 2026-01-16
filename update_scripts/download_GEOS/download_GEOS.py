import xarray as xr
import numpy as np
import os
import time


def download_geos_variable(
    var_name,
    url_template,
    update_str_GEOS,
    run_hour_GEOS,
    out_dir="data/temp",
    max_retries=5,
    wait_initial=5,
):
    """
    Download a GEOS variable via OPeNDAP with retry + zero-value detection.
    """

    url = url_template.format(
        update_str_GEOS=update_str_GEOS,
        run_hour_GEOS=run_hour_GEOS,
    )

    print(f"\n📡 GEOS OPeNDAP request for {var_name}")
    print(url)

    os.makedirs(out_dir, exist_ok=True)
    out_file = os.path.join(out_dir, f"{var_name}_GEOS_{update_str_GEOS}.nc")

    # ------------------------
    # Retry loop
    # ------------------------
    for attempt in range(max_retries):
        try:
            print(f"Attempt {attempt + 1}/{max_retries}: opening dataset")
            ds = xr.open_dataset(url, decode_times=False)

            if var_name not in ds.data_vars:
                raise KeyError(
                    f"{var_name} not found. Available variables: {list(ds.data_vars)}"
                )

            var = ds[var_name]
            print(f"{var_name}: shape {var.shape}, dtype {var.dtype}")

            size_bytes = np.prod(var.shape) * np.dtype(var.dtype).itemsize
            print(f"Approx size: {size_bytes / 1e9:.2f} GB")

            print(f"Downloading full {var_name} variable...")
            data = var.load()  # ← triggers actual OPeNDAP transfer
            ds.close()

            # ------------------------
            # CRITICAL: partial-download detection
            # ------------------------
            if np.all(data.values == 0):
                raise RuntimeError(
                    "Downloaded data are all zeros (likely OPeNDAP partial transfer)"
                )

            # Fix time encoding
            if "time" in data.coords:
                data["time"].encoding.pop("units", None)
                data["time"].attrs.pop("units", None)

            print(f"Saving validated data → {out_file}")
            data.to_netcdf(out_file, format="NETCDF4")

            print(f"✅ {var_name} download complete")
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
            "var": "ocsmass",
            "url": "http://opendap.nccs.nasa.gov:80/dods/GEOS-5/fp/0.25_deg/fcast/"
                   "tavg3_2d_aer_Nx/tavg3_2d_aer_Nx.{update_str_GEOS}_{run_hour_GEOS}",
        },
        {
            "var": "pblh",
            "url": "http://opendap.nccs.nasa.gov:80/dods/GEOS-5/fp/0.25_deg/fcast/"
                   "tavg1_2d_flx_Nx/tavg1_2d_flx_Nx.{update_str_GEOS}_{run_hour_GEOS}",
        },
        {
            "var": "v10m",
            "url": "http://opendap.nccs.nasa.gov:80/dods/GEOS-5/fp/0.25_deg/fcast/"
                   "tavg1_2d_slv_Nx/tavg1_2d_slv_Nx.{update_str_GEOS}_{run_hour_GEOS}",
        },
        {
            "var": "u10m",
            "url": "http://opendap.nccs.nasa.gov:80/dods/GEOS-5/fp/0.25_deg/fcast/"
                   "tavg1_2d_slv_Nx/tavg1_2d_slv_Nx.{update_str_GEOS}_{run_hour_GEOS}",
        },
    ]

    for item in geos_vars:
        download_geos_variable(
            var_name=item["var"],
            url_template=item["url"],
            update_str_GEOS=update_str_GEOS,
            run_hour_GEOS=run_hour_GEOS,
        )



if __name__ == "__main__":
    main()
