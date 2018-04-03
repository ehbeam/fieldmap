#!/usr/bin/python

# Created by Elizabeth Beam on 1/29/18
# Sum intermediary file results by year

import csv
import glob
import numpy
import os

# Instantiate paths
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro"
path_intermeds = "{}/networks/_intermediary".format(path)
if not os.path.isdir("{}/networks/_years".format(path)):
	os.makedirs("{}/networks/_years".format(path))

# Iterate through networks
for network in [file for file in os.listdir(path_intermeds) if not file.startswith(".")]:
	
	if (("abstracts_studies" in network) or ("results_studies" in network) or ("texts_studies" in network) or ("coordinates_studies_raw_winner-takes-all" in network)) and "func" in network:

		# Instantiate network path
		path_network = "{}/{}".format(path_intermeds, network)

		# Iterate through years
		for yr in range(1994, 2018):

			# Load intermediaries from articles published in the current year
			docs = glob.glob("{}/*{}.csv".format(path_network, str(yr)))

			# Initialize array with first document
			weights = numpy.loadtxt(open(docs[0], "r"), delimiter = ",", usecols = [2])
			nodes = []
			for line in open(docs[0], "r").readlines():
				nodes.append(tuple(line.split(",")[0:2]))

			# Iterate through documents, adding them to array
			for doc in docs[1:]:
				weights = numpy.add(weights, numpy.loadtxt(open(doc, "r"), delimiter = ",", usecols = [2]))

			# Export network for the year
			path_output = "{}/networks/_years/{}_{}.csv".format(path, network, yr)
			with open(path_output, "w+") as output:
				for i, link in enumerate(nodes):
					output.write(link[0] + "," + link[1] + "," + str(weights[i]) + "\n")

