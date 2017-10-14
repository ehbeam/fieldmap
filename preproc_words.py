#!/usr/bin/python

# Created by Elizabeth Beam on 7/28/17
# Converts PDFs to free text and preprocesses text
# Prerequisite installation:
#   - PDF Miner - https://github.com/euske/pdfminer/
# Example function call: preproc("/path/to/psychiatlas/directory", from_pdf = True)

from cStringIO import StringIO
import csv
import nltk
import os
import pandas
import PyPDF2
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter
from pdfminer.converter import TextConverter
from pdfminer.layout import LAParams
from pdfminer.pdfpage import PDFPage
import re
import string

# Function to load a list of labels
def init_labs(label_file):
    label_list = []
    with open(label_file, "r") as flab:
        lines = flab.readlines()
        for label in lines:
            label = label.translate(None, string.punctuation).lower().replace(" ", "_").replace("\n", "")
            label_list.append(label)
    return label_list

# Function to load data dictionary
def load_data(path, level):
    data_file = "{}/data/data_{}.csv".format(path, level)
    data = {}
    fdat = open(data_file, "rU")
    dat_reader = csv.DictReader(fdat)
    for dict in dat_reader:
        if level == 'experiments':
            key = dict['ID']
        else:
            key = dict['KEY']
        data.update({key: dict})
    return data

# Function to remove punctuation
def remove_punctuation(text, keep_periods = None):
    if keep_periods:
        text = text.replace(".", " . ").replace("  ", " ")
        text = "".join([char for char in text if char.isalpha() or char == "." or char == " "]).lower()
        return " ".join(text.split()) # Remove extra whitespace
    else:
        return "".join([char for char in text if char not in string.punctuation]).lower()

# Function to preprocess n-grams
def convert_ngrams(text, label_list):
    for n in range(4, 1, -1):
        ngrams = nltk.ngrams(text.split(), n)
        for ngram in ngrams:
            gramterm = " ".join(ngram)
            gramlem = "_".join(ngram)
            if gramlem in label_list:
                text = text.replace(gramterm, gramlem)
    return text

# Function to convert pdf to free text via pdfminer
# From czw (edited by Flexo) at https://stackoverflow.com/questions/5725278/how-do-i-use-pdfminer-as-a-library
def convert_pdf_to_txt(path):
    rsrcmgr = PDFResourceManager()
    retstr = StringIO()
    codec = 'utf-8'
    laparams = LAParams()
    device = TextConverter(rsrcmgr, retstr, codec=codec, laparams=laparams)
    fp = file(path, 'rb')
    interpreter = PDFPageInterpreter(rsrcmgr, device)
    password = ""
    maxpages = 0
    caching = True
    pagenos=set()
    for page in PDFPage.get_pages(fp, pagenos, maxpages=maxpages, password=password,caching=caching, check_extractable=True):
        interpreter.process_page(page)
    fp.close()
    device.close()
    str = retstr.getvalue()
    retstr.close()
    return str

# Function to write raw text
def write_raw(path, input, key, text, logfile):
    outfile_text_study = open("{}/texts/{}/raw/{}.txt".format(path, input, key), "w+")
    outfile_text_study.write(text)
    outfile_text_study.close()
    logfile.write("Extracted free text from {}\n".format(key))

# Function to write preprocessed text
def write_preproc(path, input, key, preproc, logfile):
    outfile_preproc_study = open("{}/texts/{}/preproc/{}.txt".format(path, input, key), "w+")
    outfile_preproc_study.write(preproc)
    outfile_preproc_study.close()
    logfile.write("Preprocessed text from {}\n".format(key))

# Function to aggregate preprocessed texts into a single file
def sew_preproc(path, input):
    preproc_path = "{}/texts/{}/preproc/".format(path, input)
    outfile = "{}/texts/{}/preproc_{}.txt".format(path, input, input)
    open(outfile, "w+").close()
    fout = open(outfile, "a")
    for file in filter(lambda f: not f.startswith("."), os.listdir(preproc_path)):
        fin = open(preproc_path + file, "r")
        for line in fin.readlines():
            for item in line:
                fout.write(item)
        fout.write("\n\n------------------------------\n\n")
        fin.close()
    fout.close()

# Function to dump data to outfile
def write_data(outfile, data, level):
    column_list = ['KEY', 'DISORDER', '1st_AUTHOR', 'YEAR', 'JOURNAL', 'BRAINMAP_ID', 'MEDLINE', 'ABSTRACT', 'ABSTRACT_PREPROC', 'BEHAVIORAL_DOMAIN', 'EXPERIMENT', 'MNI_COORDINATES']
    if level == 'experiments':
        column_list = ['ID'] + column_list
    with open(outfile, "w+") as fout:
        output = pandas.DataFrame(data)
        output_trans = pandas.DataFrame.transpose(output)
        output_trans.to_csv(outfile, index = False, quoting = 1, columns = column_list)

# Function for extracting text from PDFs and preprocessing
def preproc(path, input = None, from_pdf = None, level = None):
    
    # Instantiate labels
    anat_labels = "{}/labels/harvard-oxford_anatomical_labels.txt".format(path)
    behav_labels = "{}/labels/brainmap_behavioral-domain_labels.txt".format(path)
    
    # Load labels and data
    anat_list = init_labs(anat_labels)
    behav_list = init_labs(behav_labels)
    data = load_data(path, level)
    
    # Instantiate outfile for aggregated results
    if not os.path.exists("{}/texts".format(path)):
        os.makedirs("{}/texts".format(path))
    if not os.path.exists("{}/texts/{}".format(path, input)):
        os.makedirs("{}/texts/{}".format(path, input))
    if not os.path.exists("{}/texts/{}/raw".format(path, input)):
        os.makedirs("{}/texts/{}/raw".format(path, input))
    if not os.path.exists("{}/texts/{}/preproc".format(path, input)):
        os.makedirs("{}/texts/{}/preproc".format(path, input))
    outfile = "{}/texts/{}/preproc_{}.txt".format(path, input, input)
    open(outfile, "w+").close()
    
    # Instantiate log file
    logfile = "{}/logs/preproc_{}_log.txt".format(path, input)
    open(logfile, "w+").close()
    logfile = open(logfile, "a+")
    
    # Loop over studies
    for key, dict in data.iteritems():
        
        if dict['MNI_COORDINATES']:
            
            # Extract and preprocess abstracts
            if input == 'abstracts' and dict['ABSTRACT']:
                text = dict['ABSTRACT']
                preproc = remove_punctuation(text, keep_periods = True)
                preproc = convert_ngrams(preproc, anat_list + behav_list)
                data[key]['ABSTRACT_PREPROC'] = preproc
                write_raw(path, input, data[key]['KEY'], text, logfile)
                write_preproc(path, input, data[key]['KEY'], preproc, logfile)
                write_data("{}/data/data_{}.csv".format(path, level), data, level)
        
            # Otherwise, handle texts
            elif input == 'texts':
                
                # Extract full texts from pdfs
                if from_pdf:
                    pdf = "{}/texts/pdfs/{}.pdf".format(path, dict['MEDLINE'])
                    if os.path.exists(pdf):
                        try:
                            text = convert_pdf_to_txt(pdf)
                            write_raw(path, input, data[key]['KEY'], text, logfile)
                        except:
                            if not os.path.exists(outfile_text_study):
                                logfile.write("!! Error converting PDF to text for {}\n".format(key))
                            elif os.path.exists(outfile_text_study):
                                logfile.write("!! Error converting PDF to text for {}, but free text file exists\n".format(key))
                    if not data[key]['MEDLINE']:
                        logfile.write("!! No Medline ID in data for {}\n".format(key))
                    if not os.path.isfile(pdf):
                        logfile.write("!! No PDF found for {}\n".format(key))
            
                # Preprocess full texts
                file = "{}/texts/texts/raw/{}.txt".format(path, key)
                if os.path.exists(file):
                    free_lines = open(file).readlines()
                    prc_list = []
                    for line in free_lines:
                        line = line.lower().split()
                        for word in line:
                            prc_word = word.replace("/", " ").replace("-", "") # Handle special punctuation
                            prc_list.append(prc_word)
                            if word and word[-1] != "-":
                                prc_list.append(" ")
                    text = "".join(prc_list)
                    preproc = remove_punctuation(text, keep_periods = True)
                    preproc = convert_ngrams(preproc, anat_list + behav_list)
                    write_preproc(path, input, data[key]['KEY'], preproc, logfile)

        # Log omission of studies due to missing MNI coordinates
        elif not dict['MNI_COORDINATES']:
            logfile.write("!! Due to missing MNI coordinates, excluded {}\n".format(key))

    # Aggregate preprocessed texts
    sew_preproc(path, input)
    
    logfile.close()
