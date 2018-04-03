#!/usr/bin/python

# Created by Elizabeth Beam on 1/15/17
# Extract highest probability term for each coordinate and save as text file

import os
import pandas

## CogNeuro ##

# Instantiate paths
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro"
inpath = "{}/coordinates/studies/struct_56/raw".format(path)
outpath = "{}/texts/coordinates/struct_56/preproc".format(path)
if not os.path.exists(outpath):
	os.makedirs(outpath)

# Load region labels
inlab = open("{}/labels/harvard-oxford_56struct.csv".format(path), "rU").readlines()[1:]
labels = [line.split(",")[1] for line in inlab]

# Function to reformat atlasquery output labels
def gen_label(label):
	parts_to_remove = ["left_", "right_", "juxtapositional_lobule_cortex_(formerly_", ")", "_(includes_h1_and_h2"]
	for part in parts_to_remove:
		label = label.replace(part, "")
	return label

# Initialize dtm
dtm = {}

# Iterate through atlas queries from raw coordinates on the study level
for file in filter(lambda f: not f.startswith("."), os.listdir(inpath)):

	# Save preprocessed coordinates
	coords = open("{}/coordinates/studies/struct_56/raw/{}".format(path, file), "r").readlines()
	hits = [gen_label(line.split()[0]) for line in coords if gen_label(line.split()[0]) in labels]
	with open("{}/{}".format(outpath, file), "w+") as outfile:
		for hit in hits:
			outfile.write(hit + "\n")

	# Load DTM for this study
	key = file.replace(".txt", "")
	dtm[key] = {}
	dtm[key]['KEY'] = key
	for label in labels:
		dtm[key][label] = hits.count(label)

# Save DTM of anatomical regions
dtmfile = "{}/texts/coordinates/struct_56/dtm_coordinates.csv".format(path)
with open(dtmfile, "w+"):
	out = pandas.DataFrame(dtm)
	out_trans = pandas.DataFrame.transpose(out)
	out_trans.to_csv(dtmfile, index = False, quoting = 1, columns = ['KEY'] + labels)


# # Handles coordinates saved as list literals
# # Iterate through queries from raw coordinates on the study level
# for file in filter(lambda f: not f.startswith("."), os.listdir(inpath)):
# 	text = ast.literal_eval(open("{}/queries/studies/raw/{}".format(path, file), "r").read())
# 	hits = [map_all.gen_coord(coord[0][0]) for coord in text]
# 	with open("{}/{}".format(outpath, file), "w+") as outfile:
# 		for hit in hits:
# 			outfile.write(hit + "\n")
	
## Psychiatry ##

# # Instantiate paths
# path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/psychiatry"
# inpath = "{}/queries/studies/raw".format(path)
# outpath = "{}/texts/coordinates/preproc".format(path)
# if not os.path.exists(outpath):
# 	os.makedirs(outpath)

# # Iterate through queries from raw coordinates on the study level
# for file in filter(lambda f: not f.startswith("."), os.listdir(inpath)):
# 	text = open("{}/queries/studies/raw/{}".format(path, file), "r").readlines()
# 	hits = [map_all.gen_coord(line.split()[0]) for line in text]
# 	with open("{}/{}".format(outpath, file), "w+") as outfile:
# 		for hit in hits:
# 			outfile.write(hit + "\n")
