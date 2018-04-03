#!/usr/bin/python

# Created by Elizabeth Beam on February 8, 2018
# Export document-term matrices for coordinates and sections of the texts

import csv
import os
import pandas

# Instantiate path
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/psychiatry"

# Load lemma dictionary
lemfile = open("{}/vecs/lemmas_texts_100neighbors_dictionary.csv".format(path), "rU")
lemmas = {}
lemma_list = []
for dict in csv.DictReader(lemfile):
	lemmas.update({dict['TERM']: dict['LEMMA']})
	if dict['TERM'] == dict['LEMMA']:
		lemma_list.append(dict['LEMMA'])

# Iterate over inputs
for input in ["texts", "coordinates", "abstracts", "results"]:

	# Create and export document term matrix using lemmas
	dtm = {}
	files = filter(lambda f: not f.startswith("."), os.listdir("{}/texts/{}/preproc".format(path, input)))
	for file in files:
		inpath = "{}/texts/{}/preproc/{}".format(path, input, file)
		content = "  " + open(inpath).read().replace(" ", "  ").replace("\n", "  ") + "  " # Pad with spaces to distinguish nested terms
		key = file.replace(".txt", "")
		dtm[key] = {lemma: 0 for lemma in lemma_list}
		dtm[key]['KEY'] = key
		for term, lemma in lemmas.iteritems():
			dtm[key][lemma] += content.count(" " + term + " ")	
	dtmfile = "{}/texts/{}/dtm_{}.csv".format(path, input, input)
	with open(dtmfile, "w+"):
		output = pandas.DataFrame(dtm)
		output_trans = pandas.DataFrame.transpose(output)
		output_trans.to_csv(dtmfile, index = False, quoting = 1, columns = ['KEY'] + lemma_list)

	# Create and export document term matrix by year using lemmas
	for year in range(1994, 2018):
		dtm = {}
		files = filter(lambda f: str(year) in f, os.listdir("{}/texts/{}/preproc".format(path, input)))
		for file in files:
			inpath = "{}/texts/{}/preproc/{}".format(path, input, file)
			content = "  " + open(inpath).read().replace(" ", "  ").replace("\n", "  ") + "  " # Pad with spaces to distinguish nested terms
			key = file.replace(".txt", "")
			dtm[key] = {lemma: 0 for lemma in lemma_list}
			dtm[key]['KEY'] = key
			for term, lemma in lemmas.iteritems():
				dtm[key][lemma] += content.count(" " + term + " ")	
			dtmfile = "{}/texts/{}/years/dtm_{}_{}.csv".format(path, input, input, year)
			if not os.path.isdir("{}/texts/{}/years".format(path, input)):
				os.makedirs("{}/texts/{}/years".format(path, input))
			with open(dtmfile, "w+"):
				output = pandas.DataFrame(dtm)
				output_trans = pandas.DataFrame.transpose(output)
				output_trans.to_csv(dtmfile, index = False, quoting = 1, columns = ['KEY'] + lemma_list)