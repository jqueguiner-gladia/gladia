import requests
import json

def request_endpoint(url, path, header, params={}, data={}, files={}, max_retry=3):
    headers = header.copy()
    # If data is simple singular input (str/int/float/bool),
    # special header and parsing need to be applied
    if len(data) > 1:
        headers["Content-Type"] = "application/json"
        data = json.dumps(data)

    files_for_request = {key: open(value[1], "rb") for key, value in files.items()}

    response = type("", (), {})()
    response.status_code = 500
    tries = 1
    while tries <= max_retry and response.status_code != 200:

        response = requests.post(
            f"{url}{path}",
            headers=headers,
            params=params,
            data=data,
            files=files_for_request,
        )

        uploaded_files = [value[0] for key, value in files.items()]
        if isinstance(data, str):
            data = json.loads(data)
        url_files = [key for key, value in data.items() if key.endswith("_url")]
        used_files = uploaded_files + url_files
        files_message = f" ({', '.join(used_files)})"
        print(f"|  |       ___ Try : {tries}/{max_retry}{files_message}")
        print(f"|  |      |    |_ Response : {response.status_code} ")
        tries += 1
    print(f"|  |      |")

    return response

def main():
    url = "http://localhost:8080"
    path = "/text/text/language-generation/"
    header = {'Authorization': 'Bearer '}
    params = {'model': 'EleutherAI-gpt-neo-2_7B'}
    data = 'Input text to start generation from'
    request_endpoint(url, path, header, params, data)

if __name__ == "__main__":
    main()