#!/usr/bin/python

# Created by Elizabeth Beam on 7/28/17
# Aggregate BrainMap data, abstracts, and coordinates into an array
# Example function call: agg("/path/to/psychiatlas/directory")
# Data directory must contain experiments.csv, coords_mni_subject.txt, and citations.txt files

import os
import csv
import string
import itertools
import pycurl
import StringIO
import pandas

# Function to aggregate data downloaded by disorder
def agg_disorder_files(path):
    open("{}/data/citations.txt".format(path), "w+").close()
    citations = open("{}/data/citations.txt".format(path), "a")
    open("{}/data/coords.txt".format(path), "w+").close()
    coords = open("{}/data/coords.txt".format(path), "a")
    open("{}/data/experiments.csv".format(path), "w+").close()
    experiments = open("{}/data/experiments.csv".format(path), "a")
    infiles = filter(lambda f: not f.startswith("."), os.listdir("{}/data/disorders".format(path)))
    for file in infiles:
        file_path = "{}/data/disorders/{}".format(path, file)
        disorder = file.replace(".", "_").split("_")[-2].replace("-", "_")
        if file.startswith("citations"):
            for line in open(file_path, "r").readlines():
                citations.write(line)
        elif file.startswith("coords"):
            for line in open(file_path, "r").readlines():
                coords.write(line)
        elif file.startswith("experiments"):
            for line in open(file_path, "r"):
                lines = line.split("\r")
                for line in lines:
                    if line[-1] != ",":
                        line += ","
                    experiments.write(line + disorder + "\n")
    citations.close()
    coords.close()
    experiments.close()

# Function to reformat citations
def reform(infile, outfile):
    fin = open(infile, "r")
    open(outfile, "w+").close()
    fout = open(outfile, "a")
    for splitter, study in itertools.groupby(fin, lambda line: line == "\n"):
        if not splitter:
            id = ""
            study_list = list(study)
            for line in study_list:
                if line.startswith("%1"):
                    id = int(line.replace("%1 Medline Number = ", ""))
                if not line.startswith("%U"):
                    fout.write(line)
            fout.write("%M {}\n\n".format(id))
    fin.close()
    fout.close()

# Function to dump data to outfile
def write_data(outfile, data):
    column_list = ['KEY', 'DISORDER', '1st_AUTHOR', 'YEAR', 'JOURNAL', 'BRAINMAP_ID', 'MEDLINE', 'ABSTRACT', 'BEHAVIORAL_DOMAIN', 'EXPERIMENT', 'MNI_COORDINATES']
    with open(outfile, "w+") as fout:
        output = pandas.DataFrame(data)
        output_trans = pandas.DataFrame.transpose(output)
        output_trans.to_csv(outfile, index = False, quoting = 1, columns = column_list)

# Function for aggregating data into an array
def agg(path, agg_disorders = None, agg_data = None, fetch_abstracts = None, reform_citations = None):

    # Instantiate infiles
    experiments = "{}/data/experiments_act&deact.csv".format(path) # Copied from Sleuth workspace
    coordinates = "{}/data/coords.txt".format(path) # Downloaded from Sleuth
    citations = "{}/data/citations.txt".format(path) # Downloaded from Sleuth
    
    # Instantiate outfile
    outfile = "{}/data/data_act&deact.csv".format(path)

    # Initialize data dictionary
    data = dict()
    

    # Aggregate BrainMap data downloaded by disorder
    if agg_disorders:
        agg_disorder_files(path)
    
    # Aggregate data into data dictionary
    if agg_data:

        # Load experiments into data dictionary
        fexp = open(experiments, "r")
        exp_headers = ['MEETS_CRITERIA', 'BRAINMAP_ID', 'YEAR', '1st_AUTHOR', 'JOURNAL', '#', 'EXPERIMENT', 'BEHAVIORAL_DOMAIN', 'NUM_COORDINATES', 'DISORDER']
        for row in csv.reader(fexp):
            study_key = row[3] + ", " + row[2]
            study_dict = {key: value for key, value in zip(exp_headers, row)}
            if study_dict['MEETS_CRITERIA'] == "TRUE":
                if study_key in data.keys():
                    if data[study_key]['BRAINMAP_ID'] != study_dict['BRAINMAP_ID']:
                        study_key += "b"
                        data[study_key] = dict()
                elif study_key not in data.keys():
                    data[study_key] = dict()
                data[study_key]['KEY'] = study_key
                data[study_key]['YEAR'] = study_dict['YEAR']
                data[study_key]['1st_AUTHOR'] = study_dict['1st_AUTHOR']
                data[study_key]['BRAINMAP_ID'] = study_dict['BRAINMAP_ID']
                data[study_key]['JOURNAL'] = study_dict['JOURNAL']
                data[study_key]['DISORDER'] = study_dict['DISORDER']
                if 'EXPERIMENT' in data[study_key].keys():
                    data[study_key]['EXPERIMENT'] += [study_dict['EXPERIMENT']]
                if 'BEHAVIORAL_DOMAIN' in data[study_key].keys():
                    data[study_key]['BEHAVIORAL_DOMAIN'] += [study_dict['BEHAVIORAL_DOMAIN']]
                elif 'EXPERIMENT' not in data[study_key].keys():
                    data[study_key]['EXPERIMENT'] = [study_dict['EXPERIMENT']]
                elif 'BEHAVIORAL_DOMAIN' not in data[study_key].keys():
                    data[study_key]['BEHAVIORAL_DOMAIN'] = [study_dict['BEHAVIORAL_DOMAIN']]
        write_data(outfile, data)

        # Load MNI coordinates into data dictionary
        fcoord = open(coordinates, "r")
        for splitter, study in itertools.groupby(fcoord, lambda line: line == "\n"):
            if not splitter:
                study = list(study)
                for ind, line in enumerate(study):
                    if line.startswith("//") and ":" in line:
                        line = line.replace("// ", "").split(":")
                        study_key = str(line[0])
                        experiment = str(line[1])
                        if study_key in data.keys():
                            if study_key + "b" in data.keys():
                                for exp in data[study_key + "b"]['EXPERIMENT']:
                                    if exp in experiment or experiment in exp:
                                        study_key = study_key + "b"
                            if 'MNI_COORDINATES' not in data[study_key].keys():
                                data[study_key]['MNI_COORDINATES'] = set()
                            for line in study[ind+1:]:
                                if line.startswith("//"):
                                    continue
                                else:
                                    data[study_key]['MNI_COORDINATES'].add(line.replace("\t", ",").replace("\n", ""))
        write_data(outfile, data)

    # Load Medline ID and abstract into data dictionary
    if fetch_abstracts:
        fcit = open(citations, "r")
        curl = pycurl.Curl()
        store = StringIO.StringIO()
        for splitter, study in itertools.groupby(fcit, lambda line: line == "\n"):
            store.truncate(0)
            abstract = ""
            id = ""
            if not splitter:
                for line in list(study):
                    line = line.replace("\n", "")
                    if line.startswith("%D"):
                        year = line.replace("%D ", "")
                    if line.startswith("%A"):
                        author = line.split("|")[0].replace("%A ", "")
                    if line.startswith("%2"):
                        brainmap_id = line.replace("%2 BrainMap ID = ", "")
                    if line.startswith("%1"):
                        id = int(line.replace("%1 Medline Number = ", ""))
                        curl.setopt(curl.URL, "http://togows.dbcls.jp/entry/ncbi-pubmed/{}/abstract".format(id))
                        curl.setopt(pycurl.WRITEFUNCTION, store.write)
                        curl.perform()
                        abstract = store.getvalue()
                if year and author:
                    study_key = str(author + ", " + year)
                    if study_key in data.keys():
                        if study_key + "b" in data.keys():
                            if data[study_key + "b"]['BRAINMAP_ID'] == brainmap_id:
                                study_key = study_key + "b"
                    print("Fetching abstract for {}".format(study_key))
                    if id and study_key in data.keys():
                        data[study_key]['MEDLINE'] = id
                    if abstract and study_key in data.keys():
                        data[study_key]['ABSTRACT'] = abstract.replace("\n", "")
                    if not id:
                        print("No Medline ID found for {}".format(study_key))
                    if not abstract:
                        print("No abstract found for {}".format(study_key))
        write_data(outfile, data)

    # Reformat citations
    if reform_citations:
        reform(citations, "{}/data/citations_reformatted.txt".format(path))

    # Print message with path to outfile
    print("All done! Check out your data here: {}".format(outfile))


