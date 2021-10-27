import easyargs

import json
import sys

import requests
from exitstatus import ExitStatus


@easyargs
def main(url, bearer_token=''):
    print()
    header = {'Authorization': 'Bearer ' + bearer_token}
    response = requests.get(f'{url}/openapi.json', headers=header)

    endpoints = response.json()

    print(f"Testing endpoints")

    test_final_status = ExitStatus.success
    for path, details  in endpoints['paths'].items():

        print(f"|__ {path}")
        tag = details['get']['tags'][0]
        
        response = requests.get(f'{url}{path}', headers=header)
        models = response.json()
        
        status = ""
        for model in models:
            input, output, task = details['post']['tags'][0].split('.')
            status = ""
            if input == 'image':
                params = (
                    ('model', model),
                )

                files = {
                    'image': ('test.jpg', open('test.jpg', 'rb')),
                }

                response = requests.post(f'{url}{path}',  headers=header, params=params, files=files)
                
                if response.status_code == 200:
                    status = "✅"
                else:
                    status = "❌"
                    test_final_status = ExitStatus.failure
            elif input == 'text':
                params = [
                    ('model', model),
                ]
                for parameter in details['post']['parameters']:
                    if parameter['schema']['title'] != 'Model':
                        params.append((parameter['schema']['title'], parameter['schema']['default']))

                params = tuple(params)
                response = requests.post(f'{url}{path}',  headers=header, params=params)

                if response.status_code == 200:
                    status = "✅"
                else:
                    status = "❌"
                    test_final_status = ExitStatus.failure

            print(f"|  |__ {status} {model}   <{response.status_code}>")
        print("|")

    sys.exit(test_final_status)

if __name__ == '__main__':
    main()