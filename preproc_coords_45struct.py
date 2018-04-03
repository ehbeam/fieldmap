#!/usr/bin/python

# Created by Elizabeth Beam on 7/9/17
# Replaces MNI coordinates downloaded from BrainMap via Sleuth with mappings to anatomical labels from Harvard-Oxford atlas via FSL atlasquery
# Smoothing converts each coordinate to a Gaussian sphere of user-specified sigma before altasquery lookup
# Example function call: preproc("/path/to/psychiatlas/directory", smooth = True, sigma = 5)

import os
import csv
import ast
import string
import shlex
import subprocess
import operator

# Function to load data dictionary
def load_data(data_file, level):
    data = {}
    fdat = open(data_file, "rU")
    dat_reader = csv.DictReader(fdat)
    for study_dict in dat_reader:
        if level == 'experiments':
            if 'ID' in study_dict.keys():
                key = study_dict['ID']
            elif 'KEY' in study_dict.keys():
                key = study_dict['KEY']
        else:
            key = study_dict['KEY']
        data.update({key: study_dict})
    return data

# Function for extracting labels and probabilities from probabilistic Harvard-Oxford and MNI atlasquery output
def load_prob_label(proc, smooth):
    outlist = []
        
    # Parse output in format "Region1: X\n"
    if smooth:
        outlist = proc.strip().split("\n")
    
    # Parse output in format "<b>Atlas Name</b><br>X% Region1, Y% Region2"
    if not smooth and "</b><br>" in proc:
        output = proc.strip().split("</b><br>")
        outlist = output[1].split(",")
    
    label = []
    if outlist:
        for item in outlist:
            
            # Split at the percentage sign separating probabilities from labels
            if "%" in item:
                parts = item.split("%")
                prob = parts[0].strip()
                
                # Add to list with label in lowercase and unwanted punctuation removed
                lab = parts[1].strip().replace("-", "").replace(" ", "_").replace("'", "").replace("\n", "").lower()
                
                label.append([lab, prob])
        
            # Split each item in the output at the colon separating probabilities from labels
            if ":" in item:
                parts = item.split(":")
                prob = float(parts[1].strip())
                
                # Add to list with label in lowercase and unwanted punctuation removed
                lab = parts[0].strip().replace("-", "").replace(" ", "_").replace("'", "").lower()
                
                label.append([lab, prob])

    # Sort from highest to lowest probability
    label.sort(key = lambda x: float(x[1]))
    label = label[::-1]
        
    return label
    
# Function for converting MNI coordinate to voxel space for avg152T1.nii.gz
def mni_to_vox(coord):
    mni = coord.split(",")
    x = ((float(mni[0]) * -1) + 90) / 2
    y = (float(mni[1]) + 126) / 2
    z = (float(mni[2]) + 72) / 2
    return (x, y, z)

# Function for converting coordinate in voxel space to a point mask
def vox_to_mask(mask_path, vox):
    x, y, z = vox
    ext = "_{}_{}_{}".format(x, y, z)
    comm = "fslmaths {}/masks/avg152T1.nii.gz -mul 0 -add 1 -roi {} 1 {} 1 {} 1 0 1 {}/masks/mask{} -odt float".format(mask_path, x, y, z, mask_path, ext)
    args = shlex.split(comm)
    proc = subprocess.call(args)

# Function for converting point mask to a Gaussian sphere
def mask_to_gaussian(mask_path, sigma, vox):
    x, y, z = vox
    ext = "_{}_{}_{}".format(x, y, z)
    comm = "fslmaths {}/masks/mask{} -kernel gauss {} -fmean {}/masks/{}mm/gaussian{} -odt float".format(mask_path, ext, sigma, mask_path, sigma, ext)
    args = shlex.split(comm)
    proc = subprocess.call(args)

# Function for logging indeterminate coordinates
def log_indeterminate(coord, flog):
    print("Indeterminate coordinate: {}".format(coord))
    flog.write("Indeterminate coordinate \n")

# Function for generalizing Harvard-Oxford labels to form of abstract labels
def gen_label(label):
    return label.replace(",", "")

# Function for writing labels and probabilities for each coordinate to a file
def write_label(label, data, key, query, data_file):
    outfile = open(query, "w+")
    for coord in label:
        for i, item in enumerate(coord):
            item[0] = gen_label(item[0])
            item[1] = str(item[1])
            outfile.write(item[0] + " " + item[1])
            if i < len(coord) - 1:
                outfile.write(",")
        outfile.write("\n")
    outfile.close()

# Function for preprocessing coordinates
def preproc(path, smooth = None, sigma = None, level = None, mask_path = None, dual_key = None):

    # Handle optional mask path argument
    # Path to masks on Sherlock: /scratch/PI/aetkin/ebeam
    if mask_path == None:
        mask_path = path

    # Instantiate infiles
    if level == 'experiments':
        data_file = "{}/data/data_experiments.csv".format(path)
    else:
        data_file = "{}/data/data_studies.csv".format(path)
    data = load_data(data_file, level)
    labels = "{}/labels/harvard-oxford_anatomical_labels.txt".format(path)
    
    # Instantiate outfiles and outdirs
    if not os.path.exists("{}/coordinates".format(path)):
        os.makedirs("{}/coordinates".format(path))
    if not os.path.exists(mask_path):
        os.makedirs(mask_path)
    if smooth:
        log = "{}/logs/preproc_coords_{}_gaussian_{}mm_log.txt".format(path, level, sigma)
        if not os.path.exists("{}/masks/{}mm".format(mask_path, sigma)):
            os.makedirs("{}/masks/{}mm".format(mask_path, sigma))
        if level == 'experiments':
            if not os.path.exists("{}/coordinates/experiments/struct_45/gaussian_{}mm".format(path, sigma)):
                os.makedirs("{}/coordinates/experiments/struct_45/gaussian_{}mm".format(path, sigma))
        elif level != 'experiments':
            if not os.path.exists("{}/coordinates/studies/struct_45/gaussian_{}mm".format(path, sigma)):
                os.makedirs("{}/coordinates/studies/struct_45/gaussian_{}mm".format(path, sigma))
    elif not smooth:
        log = "{}/logs/preproc_coords_{}_log.txt".format(path, level)
        if level == 'experiments':
            if not os.path.exists("{}/coordinates/experiments/struct_45/raw".format(path)):
                os.makedirs("{}/coordinates/experiments/struct_45/raw".format(path))
        elif level != 'experiments':
            if not os.path.exists("{}/coordinates/studies/struct_45/raw".format(path)):
                os.makedirs("{}/coordinates/studies/struct_45/raw".format(path))
    open(log, "w+").close()
    flog = open(log, "a")

    # Replace coordinates with anatomical labels and write to file
    for key, dict in data.iteritems():

        # Initialize list of coords for the study
        label_list = []
        
        # Write a new study entry in log file
        flog.write(dict['KEY'] + " " + dict['EXPERIMENT'] + "\n\n")
        
        # Instantiate path to outfile
        sigma_dir = "raw"
        if smooth:
            sigma_dir = "gaussian_{}mm".format(sigma)
        query = "{}/coordinates/{}/struct_45/{}/{}.txt".format(path, level, sigma_dir, dict['KEY'])
        if dual_key == True:
            query = "{}/coordinates/{}/struct_45/{}/{} {}.txt".format(path, level, sigma_dir, dict['KEY'], dict['EXPERIMENT'].translate(None, string.punctuation).replace("  ", " "))
    
        # Break if study output file exists
        if os.path.isfile(query) and os.stat(query).st_size != 0:
            flog.write("Coordinate labels found at: {}\n".format(query))
            flog.write("\n------------------------------\n\n")
            continue
        
        # If no coordinates for study, move on to the next
        if dict['MNI_COORDINATES']:
            
            # Remove set literal markers
            coords = dict['MNI_COORDINATES'].replace("set([", "").replace("[[", "").replace("])", "").replace("]]", "").replace("[", "").replace("]", "").replace("'", "").split(", ")

            # Extract list of labels for each coordinate in the study set
            for coord in coords:
                
                # Write coordinate to output log
                flog.write(coord + " --> ")
                
                # If smoothing, convert coordinate to Gaussian sphere
                if smooth:
                    
                    # Convert from MNI space to voxel space
                    vox = mni_to_vox(coord)
                    x, y, z = vox
                    flog.write("{},{},{} in voxel space --> ".format(x, y, z))
                    
                    # Make a mask for the voxel coordinate if it does not exist already
                    if not os.path.isfile("{}/masks/mask_{}_{}_{}.nii.gz".format(mask_path, x, y, z)):
                        vox_to_mask(mask_path, vox)
                    if not os.path.isfile("{}/masks/{}mm/gaussian_{}_{}_{}.nii.gz".format(mask_path, sigma, x, y, z)):
                        mask_to_gaussian(mask_path, sigma, vox)
                    comm = "atlasquery -a 'Harvard-Oxford Subcortical Structural Atlas' -m {}/masks/{}mm/gaussian_{}_{}_{}".format(mask_path, sigma, x, y, z)
                
                # Else, run atlasquery on coordinate for specified atlas
                if not smooth:
                    comm = "atlasquery -a 'Harvard-Oxford Subcortical Structural Atlas' -c {}".format(coord)

                # Convert command to a list for processing
                args = shlex.split(comm)

                try:
                    proc = subprocess.check_output(args)
                        
                    # Log indeterminate coordinate
                    if proc.find("No label found!") != -1:
                        log_indeterminate(coord, flog)
                
                    # Check if label is cortical
                    else:
                        
                        # Load label from atlasquery output
                        label = load_prob_label(proc, smooth)
                        
                        # Extract top label by probability
                        top_lab = ""
                        if label and isinstance(label, list):
                            top_lab = label[0][0]
                        elif label and isinstance(label, str):
                            top_lab = label
                        
                        # If cortical, re-run atlasquery with cortical atlas
                        if "cerebral_cortex" in top_lab:
                            
                            # Run atlasquery with cortical atlas and parse output
                            if smooth:
                                comm = "atlasquery -a 'Harvard-Oxford Cortical Structural Atlas' -m {}/masks/{}mm/gaussian_{}_{}_{}".format(mask_path, sigma, x, y, z)
                            if not smooth:
                                comm = "atlasquery -a 'Harvard-Oxford Cortical Structural Atlas' -c {}".format(coord)
                            args = shlex.split(comm)
                            try:
                                proc = subprocess.check_output(args)
                                
                                # Log and write retrieved labels
                                label = load_prob_label(proc, smooth)
                                if label:
                                    flog.write("Cortical atlas results: {} \n".format(label))
                                    label_list.append(label)
                        
                            # Report failed atlasquery attempt
                            except subprocess.CalledProcessError:
                                print("Failed cortical atlasquery for {}".format(coord))
                                flog.write("Failed cortical atlasquery \n".format(coord))
                
                        # Otherwise, report subcortical labels
                        else:
                            flog.write("Subcortical atlas results: {} \n".format(label))
                            label_list.append(label)

                # Report failed atlasquery attempt
                except subprocess.CalledProcessError:
                    print("Failed atlasquery for {}".format(coord))
                    flog.write("Failed atlasquery \n".format(coord))

            # Write all labels for study to data file
            write_label(label_list, data, key, query, data_file)

        flog.write("\n------------------------------\n\n")

    # Print message with path to output
    print("All done! Check out your updated directory of atlasquery results: {}/coordinates".format(path))
    print("See here for a log of coordinate lookups: {}".format(log))

    # Say bye to outfiles
    flog.close()

# Execute script for raw coordinates
#preproc("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", smooth = None, sigma = None, level = "studies")