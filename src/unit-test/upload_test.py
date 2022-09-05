# pip install python-swiftclient python-keystoneclient

from asyncore import file_dispatcher
import swiftclient
import os
import sys
import pathlib
from pprint import pprint
from gladia_api_utils.OvhObjectStorageHelper import OVH_file_manager

file_manager = OVH_file_manager()
print(file_manager.get_objects())

examples_files = [
    "unit-test/owl2.jpg",
    "unit-test/owl2.png",
    "unit-test/owl2.gif"
]


import http.client
import mimetypes
from codecs import encode

conn = http.client.HTTPSConnection("v2-aipi.gladia.io")
dataList = []
boundary = 'wL36Yn8afVp8Ag7AmP8qZ0SA4n1v9T'
dataList.append(encode('--' + boundary))
dataList.append(encode('Content-Disposition: form-data; name=image; filename={0}'.format('test.jpg')))

fileType = mimetypes.guess_type('unit-test/owl2.jpg')[0] or 'application/octet-stream'
dataList.append(encode('Content-Type: {}'.format(fileType)))
dataList.append(encode(''))

with open('unit-test/owl2.jpg', 'rb') as f:
  dataList.append(f.read())
dataList.append(encode('--'+boundary+'--'))
dataList.append(encode(''))
body = b'\r\n'.join(dataList)
payload = body
headers = {
   'Content-type': 'multipart/form-data; boundary={}'.format(boundary) 
}
conn.request("POST", "/image/image/background-removal/?model=xception", payload, headers)
res = conn.getresponse()
data = res.read()
print(mimetypes.guess_type(data))
# print(data.decode("utf-8"))

file_manager.upload_file_from_binary(data, "png", "binary-test")