#!/usr/bin/python

# Created by Elizabeth Beam on 7/28/17
# Aggregate BrainMap data, abstracts, and coordinates into an array
# Example function call: agg("/path/to/psychiatlas/directory")
# Data directory must contain experiments.csv, coordinates.txt, and citations.txt files

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

# Function to load data dictionary
def load_data(data_file):
    data = {}
    fdat = open(data_file, "rU")
    dat_reader = csv.DictReader(fdat)
    for dict in dat_reader:
        key = dict['ID']
        data.update({key: dict})
    return data

# Function to dump data to outfile
def write_data(outfile, data):
    column_list = ['ID', 'KEY', 'DISORDER', '1st_AUTHOR', 'YEAR', 'JOURNAL', 'BRAINMAP_ID', 'MEDLINE', 'ABSTRACT', 'BEHAVIORAL_DOMAIN', 'EXPERIMENT', 'MNI_COORDINATES']
    with open(outfile, "w+") as fout:
        output = pandas.DataFrame(data)
        output_trans = pandas.DataFrame.transpose(output)
        output_trans.to_csv(outfile, index = False, quoting = 1, columns = column_list)

# Function for aggregating data into an array
def agg(path, agg_disorders = None, agg_exps = None, fetch_abstracts = None, reform_citations = None):

    # Instantiate infiles
    experiments = "{}/data/experiments.csv".format(path) # Copied from Sleuth workspace
    coordinates = "{}/data/coords.txt".format(path) # Downloaded from Sleuth
    citations = "{}/data/citations.txt".format(path) # Downloaded from Sleuth
    
    # Instantiate outfile
    outfile = "{}/data/data_experiments.csv".format(path)

    # Aggregate BrainMap data downloaded by disorder
    if agg_disorders:
        agg_disorder_files(path)
    
    # Aggregate data into data dictionary
    if agg_exps:

        # Load experiments into data dictionary
        data = {}
        fexp = open(experiments, "rU")
        exp_headers = ['MEETS_CRITERIA', 'BRAINMAP_ID', 'YEAR', '1st_AUTHOR', 'JOURNAL', '#', 'EXPERIMENT', 'BEHAVIORAL_DOMAIN', 'NUM_COORDINATES', 'DISORDER']
        key = 0
        for row in csv.reader(fexp):
            study_key = row[3] + ", " + row[2]
            study_dict = {key: value for key, value in zip(exp_headers, row)}
            if study_dict['MEETS_CRITERIA'] == "TRUE":
                data[key] = {}
                data[key]['ID'] = key
                data[key]['KEY'] = study_key
                data[key]['YEAR'] = study_dict['YEAR']
                data[key]['1st_AUTHOR'] = study_dict['1st_AUTHOR']
                data[key]['BRAINMAP_ID'] = study_dict['BRAINMAP_ID']
                data[key]['JOURNAL'] = study_dict['JOURNAL']
                data[key]['EXPERIMENT'] = study_dict['EXPERIMENT']
                data[key]['BEHAVIORAL_DOMAIN'] = study_dict['BEHAVIORAL_DOMAIN']
                data[key]['EXPERIMENT'] = study_dict['EXPERIMENT']
                data[key]['BEHAVIORAL_DOMAIN'] = study_dict['BEHAVIORAL_DOMAIN']
                if 'DISORDER' in study_dict.keys():
                    data[key]['DISORDER'] = study_dict['DISORDER']
                key += 1
        write_data(outfile, data)

        # Load MNI coordinates into data dictionary
        fcoord = open(coordinates, "r")
        for splitter, study in itertools.groupby(fcoord, lambda line: line == "\n"):
            if not splitter:
                nstudy = list(study)
                for i, line in enumerate(nstudy):
                    if line.startswith("//") and ":" in line:
                        nline = line.replace("// ", "").replace("  ", " ").split(":", 1)
                        study_key = str(nline[0])
                        experiment = str(nline[1]).strip()
                        if len(nstudy) >= i+1 and not nstudy[i+1].startswith("// Subjects"):
                            experiment += " " + nstudy[i+1].replace("// ", "").strip()
                    elif not line.startswith("//"):
                        for j, dict in data.iteritems():
                            if study_key in [dict['KEY'], dict['KEY'] + "b"] and dict['EXPERIMENT'] == experiment:
                                if 'MNI_COORDINATES' not in data[j].keys():
                                    data[j]['MNI_COORDINATES'] = []
                                data[j]['MNI_COORDINATES'].append([line.replace("\t", ",").replace("\n", "")])
        write_data(outfile, data)
        print("All done aggregating BrainMap annotations! Check out your data here: {}".format(outfile))

    # Load Medline ID and abstract into data dictionary
    if fetch_abstracts:
        data = load_data(outfile)
        fcit = open(citations, "r")
        curl = pycurl.Curl()
        store = StringIO.StringIO()
        for splitter, study in itertools.groupby(fcit, lambda line: line == "\n"):
            store.truncate(0)
            abstract, brainmap_id, medline_id = ("", "", "")
            if not splitter:
                for line in list(study):
                    line = line.replace("\n", "")
                    if line.startswith("%2"):
                        brainmap_id = line.replace("%2 BrainMap ID = ", "")
                    if line.startswith("%1"):
                        medline_id = int(line.replace("%1 Medline Number = ", ""))
                        curl.setopt(curl.URL, "http://togows.dbcls.jp/entry/ncbi-pubmed/{}/abstract".format(medline_id))
                        curl.setopt(pycurl.WRITEFUNCTION, store.write)
                        curl.perform()
                        abstract = store.getvalue()
                for key, dict in data.iteritems():
                    if brainmap_id == dict['BRAINMAP_ID']:
                        if medline_id:
                            data[key]['MEDLINE'] = medline_id
                        if abstract:
                            data[key]['ABSTRACT'] = abstract.replace("\n", "")
                if not medline_id:
                    print("No Medline ID found for BrainMap ID {}".format(brainmap_id))
                if not abstract:
                    print("No abstract found for BrainMap ID {}".format(brainmap_id))
            write_data(outfile, data)

    # Reformat citations
    if reform_citations:
        reform(citations, "{}/data/citations_reformatted.txt".format(path))


