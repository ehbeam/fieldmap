#!/usr/bin/python

# Created by Elizabeth Beam on 11/25/17
# Sums edges from documents

import os
import pandas

def sum(path, directory):

	# Instantiate infiles
	fulldir = "{}/networks/{}".format(path, directory)

	# Iterate over networks in directory
	for network in filter(lambda f: not f.startswith(".") and not f.endswith(".csv"), os.listdir(fulldir)):
		
		# Initialize data dictionary
		data = {}

		# Loop over indir files and write to outfile
		for file in filter(lambda f: not f.startswith("."), os.listdir("{}/{}".format(fulldir, network))):
			fin = open("{}/{}/{}".format(fulldir, network, file), "r")
			for i, line in enumerate(fin.readlines()):
				items = line.strip().split(",")
				if i not in data.keys():
					data[i] = {'NODE1': "", 'NODE2': "", 'WEIGHT': 0}
				data[i]['NODE1'] = items[0]
				data[i]['NODE2'] = items[1]
				data[i]['WEIGHT'] += float(items[2])

		outfile = open("{}/networks/{}/{}.csv".format(path, directory, network), "w+")
		for i in data.keys():
			outfile.write(data[i]['NODE1'] + "," + data[i]['NODE2'] + "," + str(data[i]['WEIGHT']) + "\n")

		# Close outfile
		outfile.close()

sum("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", "_control/_intermediary")