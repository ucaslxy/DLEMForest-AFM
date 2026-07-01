# -*- coding: utf-8 -*-
# author: xiaoyong.li@sdut.edu.cn

import time
import os
import numpy as np
from gwp_image import IMAGE
import pandas as pd
import math


# Keep original data reading and writing functions
def readImage(vfile):
    """Read raster data and return data, rows/cols, projection, and geotransform parameters"""
    if not os.path.exists(vfile):
        raise FileNotFoundError(f"File not found: {vfile}")
        
    drv = IMAGE()
    im_proj, im_geomtrans, im_data = drv.read_img(vfile)
    
    # Check data dimensions
    if len(im_data.shape) == 2:
        row, col = im_data.shape
    elif len(im_data.shape) == 3:
        band, row, col = im_data.shape
    else:
        raise ValueError(f"Unsupported raster dimension: {im_data.shape}")
        
    return im_data, row, col, im_proj, im_geomtrans


def readImageL(vfile):
    """Read raster data and return only the data array"""
    if not os.path.exists(vfile):
        raise FileNotFoundError(f"File not found: {vfile}")
        
    drv = IMAGE()
    im_proj, im_geomtrans, im_data = drv.read_img(vfile)
    return im_data


def readImageM(vfile):
    """Read multi-band raster data"""
    if not os.path.exists(vfile):
        raise FileNotFoundError(f"File not found: {vfile}")
        
    drv = IMAGE()
    im_proj, im_geomtrans, im_data = drv.read_img(vfile)
    band, row, col = im_data.shape
    return im_data, band, row, col


def writeImage(fileName, im_proj, im_geotrans, im_data, original_rows, original_cols):
    """Write raster data and adjust geotransform parameters based on aggregation ratio"""
    # Calculate aggregation ratio
    row_ratio = original_rows / im_data.shape[0]
    col_ratio = original_cols / im_data.shape[1]
    
    # Adjust geotransform parameters
    new_geotrans = list(im_geotrans)
    new_geotrans[1] *= col_ratio  # Adjust x-direction resolution
    new_geotrans[5] *= row_ratio  # Adjust y-direction resolution
    
    # Create output directory if it does not exist
    out_dir = os.path.dirname(fileName)
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)
    
    drv = IMAGE()
    drv.write_img(fileName, im_proj, new_geotrans, im_data)


def aggregate_raster_numpy(raster_data, window_size=(10, 10)):
    """Aggregate raster data using windowed mean via NumPy"""
    # Check divisibility of input dimensions by window size
    rows, cols = raster_data.shape
    target_rows, target_cols = rows // window_size[0], cols // window_size[1]
    
    # Handle cases where dimensions are not perfectly divisible
    rows_trim = (rows % window_size[0])
    cols_trim = (cols % window_size[1])
    
    if rows_trim > 0 or cols_trim > 0:
        print(f"Warning: Raster dimensions ({rows}, {cols}) not divisible by window size {window_size}, will be cropped")
        raster_data = raster_data[:-rows_trim, :-cols_trim]
    
    # Reshape data and calculate mean
    reshaped_data = raster_data.reshape(
        target_rows, window_size[0], 
        target_cols, window_size[1]
    )
    aggregated_data = np.mean(reshaped_data, axis=(1, 3))
    
    return aggregated_data


def allocate_by_area(state_cell_loc, state_probs, state_areas, target_area, state_id, year):
    """
    Improved allocation function to address insufficient supply.
    Allocates all possible cells when available area is insufficient.
    """
    # Calculate total available resources in the state
    total_possible_area = np.sum(state_areas)
    valid_cells = np.sum(state_probs > 0)
    valid_area = np.sum(state_areas[state_probs > 0])
    
    # Diagnostic output
    print(f"  State {state_id} resources: Valid cells={valid_cells}, Valid area={valid_area:.2f}km², Target={target_area:.2f}km²")
    
    # Case 1: No valid cells
    if valid_cells == 0:
        return [], 0.0, "No valid cells"
    
    # Case 2: Available area is less than target (core issue of shortage)
    if valid_area < target_area:
        shortage = target_area - valid_area
        print(f"  Warning: State {state_id} area insufficient, gap {shortage:.2f}km², will allocate all valid cells")
        # Adjust target to the maximum available area
        target_area = valid_area
    
    # Sort by probability descending
    sorted_indices = np.argsort(-state_probs)
    sorted_probs = state_probs[sorted_indices]
    sorted_areas = state_areas[sorted_indices]
    
    # Accumulate area until target is met
    cumulative_area = 0.0
    allocated_indices = []
    
    # Relax filtering conditions to allow low-probability cells to be selected
    min_prob_threshold = max(np.min(sorted_probs[sorted_probs > 0]) * 0.5, 0.001)
    
    for i in range(len(sorted_probs)):
        if cumulative_area >= target_area:
            break
            
        # Relax condition: no longer skip all low-probability cells, only those near zero
        if sorted_probs[i] < min_prob_threshold:
            continue
            
        cumulative_area += sorted_areas[i]
        allocated_indices.append(sorted_indices[i])  # Record original index
    
    # Calculate final difference
    diff = cumulative_area - target_area
    status = f"Allocation complete (Diff: {diff:.2f}km²)" if abs(diff) < 0.1 else f"Significant allocation gap: {diff:.2f}km²"
    
    return allocated_indices, cumulative_area, status


def main():
    try:
        start_time = time.time()
        
        # Read demand data
        print("Reading input data...")
        pf_file = "D:/DLEM_FM/0input/pfmap_v2/pf_map_1900_2020_v2.xlsx"
        pf_pd = pd.read_excel(pf_file, sheet_name='Sheet1', engine='openpyxl')
        pf_np = pf_pd.to_numpy()
        pf_demand = pf_np[:, 1:]  # Assume unit is km²
        
        # State mask
        print("Reading state mask data...")
        SID_array = readImageL("D:/DLEM_FM/0base/seus_state_mask_30s.tif")
        tmp, row, col, im_proj, im_geomtrans = readImage("D:/DLEM_FM/0base/seus_state_mask_30s.tif")
        
        # Print geotransform parameters
        print(f"Geotransform parameters: {im_geomtrans}")
        
        # Read true area raster (unit: km²)
        print("Reading true area raster data...")
        area_raster = readImageL("D:/DLEM_FM/0input/landarea/land_area_30s_albersm.tif")
        
        # Verify area raster dimensions
        if area_raster.shape != (row, col):
            raise ValueError(f"Area raster dimensions ({area_raster.shape}) inconsistent with state mask ({row}, {col})")
        
        # Verify area data validity
        if np.any(area_raster <= 0):
            invalid_count = np.sum(area_raster <= 0)
            print(f"  Warning: Found {invalid_count} cells with area <= 0, replacing with minimum")
            min_valid_area = np.min(area_raster[area_raster > 0])
            area_raster[area_raster <= 0] = min_valid_area
        
        # Planting forest probability and max suitability
        print("Reading forest planting probability data...")
        pf_frac = readImageL("D:/DLEM_FM/0input/tmp/pf_seus_30s_e.tif")
        pf_max = readImageL("D:/DLEM_FM/0input/cohort/tmp/max_pf_seus_binary_m.tif")
        pf_prob = pf_frac * pf_max
        
        # Adjust probability threshold to increase available cells
        print(f"  Original probability range: Min={np.min(pf_prob):.4f}, Max={np.max(pf_prob):.4f}")
        # Lower probability threshold to allow more cells to be selected
        min_p = np.percentile(pf_prob[pf_prob > 0], 5)  # 5th percentile
        pf_prob[pf_prob < min_p] = 0  # Filter out extremely low probabilities
        print(f"  Adjusted probability range: Min={np.min(pf_prob[pf_prob > 0]):.4f}, Max={np.max(pf_prob):.4f}")
        
        # Verify data dimension consistency
        if pf_prob.shape != (row, col):
            raise ValueError(f"Probability data dimensions ({pf_prob.shape}) inconsistent with mask ({row}, {col})")
        
        if SID_array.shape != (row, col):
            raise ValueError(f"State mask dimensions ({SID_array.shape}) inconsistent with expected dimensions ({row}, {col})")
        
        # Check number of states and year range
        num_states, num_years = pf_demand.shape
        print(f"Processing {num_states} states for {num_years} years")
        
        # Spatial allocation
        print("Starting spatial allocation...")
        for yr in range(1901, 2021):
            year_start_time = time.time()
            year_idx = yr - 1900
            print(f"\nProcessing year: {yr} ({year_idx+1}/{num_years})")
            
            pf_array = np.zeros([row, col], dtype=np.int8)
            total_target = 0.0
            total_actual = 0.0
            
            for state_id in range(1, num_states + 1):
                state_idx = state_id - 1
                target_area = pf_demand[state_idx, year_idx]
                total_target += target_area
                
                # Print progress
                if state_id % 5 == 0:
                    print(f"  Processing state: {state_id}/{num_states}, Target area: {target_area:.2f}km²")
                
                # Skip state if target area is 0
                if target_area <= 0:
                    continue
                
                # Get state cell locations
                state_cell_loc = np.where(SID_array == state_id)
                cell_count = len(state_cell_loc[0])
                
                if cell_count == 0:
                    print(f"  Warning: State {state_id} has no cells in the mask data")
                    continue
                
                # Get probability and area data for the state
                state_probs = pf_prob[state_cell_loc]
                state_areas = area_raster[state_cell_loc]
                
                # Allocate cells
                allocated_indices, actual_area, status = allocate_by_area(
                    state_cell_loc, state_probs, state_areas, target_area, state_id, yr
                )
                
                total_actual += actual_area
                
                # Mark allocated cells
                for idx in allocated_indices:
                    x_loc = state_cell_loc[0][idx]
                    y_loc = state_cell_loc[1][idx]
                    pf_array[x_loc, y_loc] = 1
                
                # Output status
                print(f"  State {state_id}: {status}, Target={target_area:.2f}km², Actual={actual_area:.2f}km²")
            
            # Annual summary
            print(f"\nYear {yr} summary:")
            print(f"  Total target area: {total_target:.2f}km²")
            print(f"  Total actual area: {total_actual:.2f}km²")
            print(f"  Overall completion rate: {(total_actual/total_target)*100:.2f}%" if total_target > 0 else "  No target area")
            
            # Save results
            print(f"  Saving {yr} results...")
            
            pf_array_r = pf_array.astype(np.int8)
            original_rows, original_cols = pf_array_r.shape
            pf_array_5min = aggregate_raster_numpy(pf_array_r, window_size=(10, 10))
            outName = f"D:/DLEM_FM/0input/pfmap_v3/fpltf_{yr}.tif"
            writeImage(outName, im_proj, im_geomtrans, pf_array_5min, original_rows, original_cols)
            
            # Calculate processing time for the year
            year_elapsed = time.time() - year_start_time
            print(f"  Year {yr} processing complete, duration: {year_elapsed:.2f}s")
        
        # Calculate total running time
        total_elapsed = time.time() - start_time
        print(f"\nAll years processed! Total duration: {total_elapsed:.2f}s")
        
    except Exception as e:
        print(f"Error occurred: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()