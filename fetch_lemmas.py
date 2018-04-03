#!/usr/bin/python

# Created by Elizabeth Beam on 10/15/17
# Compiles raw lemma dictionary from output of word2vec
# Example function call: agg("/path/to/psychiatlas/directory", "texts", 100)

from collections import OrderedDict
import itertools
import os
import pandas
import string
import sys

# Function to load a list of labels
def init_labs(label_file):
    label_list = []
    with open(label_file, "r") as flab:
        lines = flab.readlines()
        for label in lines:
            label = label.translate(None, string.punctuation).lower().replace(" ", "_").replace("\n", "")
            label_list.append(label)
    return label_list

def agg(path, input, n_neighbors, n_epochs):

    # Instantiate outfile
    outfile = "{}/embeddings/lemmas_{}_{}neighbors_{}epochs.csv".format(path, input, n_neighbors, n_epochs)
    fout = open(outfile, "a+")

    # Instantiate infile and exit if it does not exist
    infile = "{}/embeddings/neighbors_{}_{}neighbors_{}epochs.txt".format(path, input, n_neighbors, n_epochs)
    if not os.path.isfile(infile):
        print("The word2vec output file does not exist! Run word2vec with the -i flag, then run model.nearby([terms]) and save as {}".format(infile))
        sys.exit()
    fin = open(infile, "r")

    # Load anatomical and behavioral labels
    anat_labels = init_labs("{}/labels/harvard-oxford_anatomical_labels.txt".format(path))
    behav_labels = init_labs("{}/labels/brainmap_behavioral-domain_labels.txt".format(path))

    # Initialize dictionary for output
    data = {}

    # Load output dictionary
    key = 0
    for splitter, lines in itertools.groupby(fin, lambda line: line == "\n"):
        if not splitter:
            lines = [line.replace("\n", "") for line in list(lines)]
            lemma = lines[0]
            for line in lines[2:]:
                key += 1
                data[key] = {}
                data[key]['LEMMA'] = lemma
                data[key]['TERM'] = line.split()[0]
                data[key]['DISTANCE'] = line.split()[1]
                if data[key]['TERM'] == lemma:
                    data[key]['SELECTED'] = "yes"
                if lemma in anat_labels:
                    data[key]['CLASS'] = "anatomy"
                elif lemma in behav_labels:
                    data[key]['CLASS'] = "behavior"

    # Sort and save output
    data = OrderedDict(sorted(data.items()))
    column_list = ['CLASS', 'LEMMA', 'TERM', 'DISTANCE', 'SELECTED']
    with open(outfile, "w+") as fout:
        output = pandas.DataFrame(data)
        output_trans = pandas.DataFrame.transpose(output)
        output_trans.to_csv(fout, index = False, quoting = 1, columns = column_list)

agg("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", "texts", 100)