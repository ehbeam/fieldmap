#!/usr/bin/python

# Created by Elizabeth Beam on 11/1/17
# Scrambles the words and periods in a corpus of preprocessed documents
# Example function call: scramble("/path/to/psychiatlas/directory", "texts")

import map_all
import os
from random import shuffle

# Function to execute text scrambling
def scramble(path, level, input):

	# Instantiate paths for inputs and outputs
	corpus = "{}/texts/{}/preproc_{}.txt".format(path, input, input)
	input_path = "{}/texts/{}/preproc".format(path, input)
	output_path = "{}/texts/{}/scrambled/{}".format(path, input, level)
	if not os.path.exists(output_path):
		os.makedirs(output_path)

	# Initialize a list of words in the corpus
	words = []

	# Load all words from the corpus
	for line in open(corpus, "r").readlines():
		words += [word for word in line.split() if word != "------------------------------"]
	
	# Shuffle words in place
	shuffle(words)

	# Write scrambled corpus to outfile
	outfile = open("{}/texts/{}/scrambled_{}.txt".format(path, input, input), "w+")
	outfile.writelines([word + " " for word in words])
	outfile.close()

	# Write partitioned words to files
	if level == "experiments":
		n = 5227
	if level == "studies":
		n = len(filter(lambda f: not f.startswith("."), os.listdir(input_path)))
	split_words = [words[i::n] for i in xrange(n)]
	for i, partition in enumerate(split_words):
		outfile = open("{}/{}.txt".format(output_path, i), "w+")
		outfile.writelines([word + " " for word in partition])

# Parameters for mapping
levels = ["studies"] #["experiments", "studies"]
inputs = ["results"]#["results", "abstracts", "texts"]
lemmas = [True]#[True, None]

# Execute scrambling of all text inputs
for lev in levels:
	for inp in inputs:
		scramble("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", lev, inp)

# Execute mapping of all text inputs and parameters
for inp in inputs:
	for lem in lemmas:
		map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", inp, anat = True, lemmas = lem, neighbors = 100, level = "studies", window = "sentences", force = True, scrambled = True)
		map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", inp, behav = True, lemmas = lem, neighbors = 100, level = "studies", window = "sentences", force = True, scrambled = True)
		map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", inp, func = True, lemmas = lem, neighbors = 100, level = "studies", window = "sentences", force = True, scrambled = True)