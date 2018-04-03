#!/usr/bin/python

# Created by Elizabeth Beam on 11/6/17
# Aggregate data from Lisa McTeague, abstracts, and coordinates into an array
# Example function call: agg("/path/to/psychiatlas/directory")
# Data directory must contain data.dat and /path/texts/pdfs must contains pdfs

import csv
import pandas

# Function to load data dictionary
def load_data(path, level):
    data = {}
    data_file = open("{}/data/data_{}.csv".format(path, level), "rU")
    dat_reader = csv.DictReader(data_file)
    for dict in dat_reader:
        data.update({dict['KEY']: dict})
    return data

# Instantiate path
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/psychiatry"

# Fields to load into data
fields = ['SOURCE', 'KEY', '1st_AUTHOR', 'YEAR', 'JOURNAL', 'BRAINMAP_ID', 'MEDLINE', 'ABSTRACT', 'ABSTRACT_PREPROC', 
			'BEHAVIORAL_DOMAIN', 'EXPERIMENT', 'MNI_COORDINATES', 'NUM_COORDINATES', 'NUM_SUBJECTS', 'AUTHORS', 
			'NUM_AUTHORS', 'IMPACT_FACTOR', 'IMPACT_FACTOR_WITHOUT_SELFCITES', 'IMPACT_FACTOR_PERCENTILE', 'IMMEDIACY_INDEX', 
			'EIGENFACTOR', 'NORMALIZED_EIGENFACTOR', 'JOURNAL_CITATIONS', 'ARTICLE_CITATIONS', 'ARTICLE_CITATIONS_PER_YEAR']

# Load BrainMap data keyed by Pubmed ID
brainmap = {}
for dict in csv.DictReader(open("{}/data/brainmap_data.csv".format(path), "rU")):
		key = dict['MEDLINE']
		brainmap[key] = {}
		for field in fields[1:12]:
			brainmap[key][field] = dict[field]
		for field in fields[12:]:
			brainmap[key][field] = ""
		brainmap[key]['SOURCE'] = "BrainMap"

# Load McTeague data keyed by Pubmed ID
mcteague = {}
for dict in csv.DictReader(open("{}/mcteague/biblio/data_studies_metadata.csv".format(path), "rU")):
	key = dict['MEDLINE']
	mcteague[key] = {}
	for field in fields[2:]:
		mcteague[key][field] = dict[field]
	mcteague[key]['1st_AUTHOR'] = dict['AUTHORS'].split(";")[0].replace(",", "").replace(".", "")
	mcteague[key]['KEY'] = mcteague[key]['1st_AUTHOR'] + ", " + dict['YEAR']
	mcteague[key]['SOURCE'] = "McTeague"

# Initialize integrated data dictionary
combo = {}

# Load data from McTeague first, as it will include bibliometrics
for key, dict in mcteague.items():
	combo[key] = dict
for key, dict in brainmap.items():
	if key not in combo.keys():
		combo[key] = dict

# Check that there are no duplicate keys
keys = []
for dict in combo.values():
	if dict['KEY'] in keys:
		combo[dict['MEDLINE']]['KEY'] += "b"
	keys.append(dict['KEY'])
keys = []
for dict in combo.values():
	if dict['KEY'] in keys:
		print("Duplicate key: {}".format(dict['KEY']))
	keys.append(dict['KEY'])

# Dump data to outfile
outfile = "{}/data/data_studies.csv".format(path)
with open(outfile, "w+") as fout:
        output = pandas.DataFrame(combo)
        output_trans = pandas.DataFrame.transpose(output)
        output_trans.to_csv(outfile, index = False, quoting = 1, columns = fields)
