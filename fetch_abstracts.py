#!/usr/bin/python

# Created by Elizabeth Beam on 2/16/18
# Download abstracts of all fMRI studies of humans and export by year

from Bio import Entrez, Medline
import itertools
import preproc_words
import os
import glob

# Instantiate paths
inpath = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro"
outpath = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/web"
if not os.path.exists("{}/abstracts".format(outpath)):
	os.makedirs("{}/abstracts".format(outpath))
else:
	for file in glob.glob("{}/abstracts/*".format(outpath)):
		os.remove(file)

# Load labels
anat_labels = "{}/labels/harvard-oxford_anatomical_labels.txt".format(inpath)
behav_labels = "{}/labels/brainmap_behavioral-domain_labels.txt".format(inpath)
anat_list = preproc_words.init_labs(anat_labels)
behav_list = preproc_words.init_labs(behav_labels)

# Initiate search query
Entrez.email = "ebeam@stanford.edu" 
query = "(fmri OR functional mri) AND (brain OR neuro* OR psych*) AND human[species]"

# Iterate over batches
for start in [1, 100001]:

	# Execute search
	handle = Entrez.esearch(db = "pubmed", term = query, retmode = "text", retstart = start, retmax = 100000)
	record = Entrez.read(handle) 

	# Save output to file keyed by year
	for rec_id in record['IdList']:
		try:
			rec = Entrez.efetch(db = "pubmed", id = rec_id, rettype = "Medline", retmode = "text").read()
			year = rec.split("DP  - ")[1].split()[0]
			text = preproc_words.convert_ngrams(preproc_words.remove_punctuation(rec, keep_periods = True), anat_list + behav_list)
			text = text.replace("\n", " ").replace("\t", " ").replace("  ", " ")
			file = open("{}/abstracts/{}.txt".format(outpath, year), "a+")
			file.write(text + "\n")
		except:
			print("Failed lookup for {}".format(rec_id))
