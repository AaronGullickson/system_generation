# We will use the geonames database to select a set of place names
# for each country from the full list. This will become the basis for 
# our random place name generator by nationality.

# http://www.geonames.org/export/
# http://download.geonames.org/export/dump/readme.txt
# http://www.geonames.org/export/codes.html

# Because of the size of this database, we will read it directly from URL
# rather than download it. R was choking on the data, so I switched over to 
# a python script. The big if command below indicates all the codes that 
# we are currently including. 

import os
import gzip
import shutil
import requests
import io
import zipfile

f1 = open('./place_names.tsv', 'w+')

response = requests.get('http://download.geonames.org/export/dump/allCountries.zip')

seperator = "\t"
with zipfile.ZipFile(io.BytesIO(response.content)) as z:
    for filename in z.namelist():
      if(filename =='allCountries.txt'):
        with z.open(filename) as f:
          for line in f:
            fields = line.split("\t")
            feature_name = fields[1]
            feature_class = fields[6]
            feature_code = fields[7]
            feature_country = fields[8]
            population = fields[14]
            if(feature_class=="A" or feature_class=="P" 
#            or feature_code=="BAY" or feature_code=="BAYS" or feature_code=="COVE"
#            or feature_code=="ESTY" or feature_code=="GULF" or feature_code=="LK"
#            or feature_code=="LKS" or feature_code=="SD" or feature_code=="SEA" 
#            or feature_code=="STM"
#            or feature_code=="CST" or feature_code=="OAS" or feature_code=="PRK"
#            or feature_code=="RES" or feature_code=="RESV" or feature_code=="RGN"
#            or feature_code=="RGNH" or feature_code=="TRB"
#            or feature_code=="ANS" or feature_code=="RLG"
#            or feature_code=="BCH" or feature_code=="BDLD" or feature_code=="BUTE"
#            or feature_code=="CAPE" or feature_code=="CNYN" or feature_code=="DSRT"
#            or feature_code=="GRGE" or feature_code=="ISL" or feature_code=="ISLET"
#            or feature_code=="ISLS" or feature_code=="ISTH" or feature_code=="MT"
#            or feature_code=="MTS" or feature_code=="PEN" or feature_code=="SHOR"
#            or feature_code=="VAL" or feature_code=="VALS" or feature_code=="VALG"
#            or feature_code=="VLC"
             or feature_code=="PK"):
              print >>f1, seperator.join([feature_name,feature_class,feature_code,feature_country,population])
            
f1.close()    

#gzip it
with open('place_names.tsv', 'rb') as f_in, gzip.open('place_names.tsv.gz', 'wb') as f_out:
    shutil.copyfileobj(f_in, f_out)
os.remove('place_names.tsv')
