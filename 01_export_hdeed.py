import xarray as xr
import numpy as np
import pandas as pd

# Open the Zarr dataset once
ds = xr.open_zarr('/Volumes/External/Dheed/mergedlabels_ranked_pot0.01_ne0.1_cmp_S1_T3_1950_2023.zarr/')

# Loop over each year
for year in range(2000, 2024):  # 2024 is exclusive
    print(f"Processing year: {year}")
    
    # Subset the data for the current year
    ds_subset = ds.sel(
        longitude=slice(180, 310),
        latitude=slice(70, 20),
        Ti=slice(np.datetime64(f'{year}-01-01'), np.datetime64(f'{year}-12-31'))
    )

    # Convert to DataFrame
    df = ds_subset.to_dataframe().reset_index()

    # Drop rows where 'labels' == 0
    df = df[df['labels'] != 0]

    # Save to CSV
    output_path = f'/Volumes/External/Dheed/NA_{year}.csv'
    df.to_csv(output_path, index=False)
    
    print(f"Saved {output_path}")
