#!/usr/bin/python

# Created by Elizabeth Beam on 11/2/17
# Scrambles order of coordinates, keeping probabilistic labels within coordinates,
# and randomly partitioning (n = number of files at the user-specified level)
# Example function call: scramble("/path/to/psychiatlas/directory", "studies", 0, aggregate = None)

import ast
import itertools
import map_all
import os
from random import shuffle

# Function to execute text scrambling
def scramble(path, level, sigma, aggregate = None):

	# Load name for sigma level
	if sigma == 0:
		sigma_name = "raw"
	if sigma > 0:
		sigma_name = "gaussian_{}mm".format(sigma)

	# Set paths
	aggregate_file = "{}/queries/{}/preproc_{}.txt".format(path, level, sigma_name)
	query_path = "{}/queries/{}/{}".format(path, level, sigma_name)
	output_path = "{}/queries/{}/scrambled/{}".format(path, level, sigma_name)
	if not os.path.exists(output_path):
		os.makedirs(output_path)

	if aggregate:

		# Reset file of aggregated coordinates and open for appending
		open(aggregate_file, "w+")
		aggregate = open(aggregate_file, "a")

	    # Aggregate coordinates
		for file in filter(lambda f: not f.startswith("."), os.listdir(queries)):
			for line in open(query_path + "/" + file, "r").readlines():
				for item in line:
					aggregate.write(item)
			aggregate.write("\n\n------------------------------\n\n")

	# Initialize a list of words in the corpus
	coords = []

	# Set number of partitions
	n = len(filter(lambda f: not f.startswith("."), os.listdir(query_path)))

	# Load all words from the corpus
	for splitter, group in itertools.groupby(open(aggregate_file, "r"), lambda line: line == "\n" or "------------------------------" in line):
		if not splitter:
			grouped_coords = ast.literal_eval(list(group)[0])
			coords += [coord for coord in grouped_coords]

	# Scramble coordinates in place
	shuffle(coords)

	# Divide coordinates into roughly equal partitions and write to files
	split_coords = [coords[i::n] for i in xrange(n)]
	for i, partition in enumerate(split_coords):
		outfile = open("{}/{}.txt".format(output_path, i), "w+")
		outfile.write(str(partition))

# Execute scrambling over all parameters
levels = ["studies", "experiments"]
sigmas = [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]
for level in levels:
	for sigma in sigmas:
		scramble("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", level, sigma)
