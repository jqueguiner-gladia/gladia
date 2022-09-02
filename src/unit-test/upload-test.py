# pip install python-swiftclient python-keystoneclient

import swiftclient
import os
import sys
import pathlib
from pprint import pprint

SWIFT_USERNAME = "user-qvchayKnVQeZ"
SWIFT_KEY = "df7X2UPtha4qW5SNDkmpRnsH595SVWMm"
SWIFT_AUTH_URL = "https://auth.cloud.ovh.net/v3"
SWIFT_CONTAINER_NAME = "filehosting"
REGION_NAME = "GRA"
TENANT_NAME = "8938870676049710"
CURRENT_DIRECTORY = os.path.dirname(os.path.abspath(__file__))

def get_objects():
    raw_result = conn.get_container(SWIFT_CONTAINER_NAME, prefix="examples/")[1]
    list_objects = [res["name"] for res in raw_result]
    return list_objects


# initiate a Swift service connection
conn = swiftclient.Connection(
    user=SWIFT_USERNAME,
    key=SWIFT_KEY,
    authurl=SWIFT_AUTH_URL,
    tenant_name=TENANT_NAME,
    auth_version=SWIFT_AUTH_URL[-1:],
    os_options={
        "region_name": REGION_NAME,
    }
)
# upload file to Swift storage
files_to_upload={
    "testocr": "/image/text/ocr/"
}
content_type={
    ".jpg": "image/jpeg",
    ".jpeg": "image/jpeg",
    ".gif": "image/gif",
    ".png": "image/png",
    ".mp3": "audio/mpeg",
    ".wav": "audio/wav",
    ".m4a": "audio/x-m4a"
}

# delete file from Swift storage
files_to_delete = [
    "examples/image/text/ocr/testocr.jpg"
]
for file in files_to_delete:
    conn.delete_object(SWIFT_CONTAINER_NAME, file) 

    
files_path = os.path.join(CURRENT_DIRECTORY)
files = os.listdir(files_path)
for name_file, task in files_to_upload.items():
    for file in files:
        if file.startswith(name_file):
            file_path = os.path.join(files_path, file)
            file_path = file_path.replace("/app/", "")
            new_file_name = "examples" + task + file
            file_exention = os.path.splitext(file_path)[-1]
            with open(file_path, 'rb') as file:
                try:
                    conn.put_object(SWIFT_CONTAINER_NAME, new_file_name,
                                    contents=file.read(),
                                    content_type=content_type[file_exention])
                except Exception as e:
                    print("error with:", file_path, "Error message:", e)




pprint(get_objects())
