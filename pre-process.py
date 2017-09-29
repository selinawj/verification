import json
import pandas as pd
import numpy as np

df = pd.read_csv('renthop.csv')

#get id
id = df.ix[:,1]

posterData=[]
carrierData=[]
licenseData=[]
phoneData=[]
psData=[]
prepostBlob = df.ix[:,54]
for each in prepostBlob:
    each = json.loads(each)

    #poster type
    if 'purpose_type' in each:
        poster = each['purpose_type']
        poster = str(poster)
        posterData.append(poster)
    else: #checks if key is empty
        poster = ""
        posterData.append(poster)

    #carrier type
    if 'phone_lookup' in each:
        carrier = each['phone_lookup']['carrier_type']
        carrier = str(carrier)
        carrierData.append(carrier)
    else: #checks if key is empty
        carrier = ""
        carrierData.append(carrier)

    #agent license
    license = each['additional-details-agent']
    license = str(license)
    licenseData.append(license)
    
    #phone num
    if 'phone_lookup' in each:
        phoneNum = each['phone_lookup']['number']
        phoneNum = str(phoneNum)
        phoneData.append(phoneNum)
    else: #checks if key is empty
        phoneNum = ""
        phoneData.append(phoneNum)
    
    #posting state
    ps = each['ip_geolocation']['city']
    ps = str(ps)
    psData.append(ps)

#write to csv with column order specified
detailsBlob = pd.DataFrame({'poster': posterData, 'carrier': carrierData, 'license': licenseData, 'phoneNum': phoneData, 'postingState': psData}, columns = ['poster', 'license', 'carrier', 'phoneNum', 'postingState'])
detailsBlob.to_csv('details.csv', index=False, header=False)
