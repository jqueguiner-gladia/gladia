import json
import os
import sys
from time import sleep

import click
import requests

global nb_total_tests
global nb_test_ran, nb_test_passed, nb_test_failed, nb_test_skipped
global test_final_status
global endpoints

STATUS_PASSED = "🟢"
STATUS_SKIPPED = "🟡"
STATUS_FAILED = "🔴"

EXIT_STATUS_SUCCESS = 0
EXIT_STATUS_FAILURE = 1
CURRENT_DIRECTORY = os.path.dirname(os.path.abspath(__file__))


def get_nb_tests(
    url,
    header,
    endpoints,
    specific_endpoints=[],
    specific_models="",
    default_models_only=False,
):
    nb_total_tests = 0
    for path, _ in endpoints["paths"].items():
        models = get_endpoints_models(url, path, header)
        if specific_endpoints:
            if path in specific_endpoints:
                if specific_models:
                    nb_total_tests += len(
                        [model for model in models if model in specific_models]
                    )
                elif default_models_only:
                    nb_total_tests += 1
                else:
                    nb_total_tests += len(models)
        elif specific_models:
            nb_total_tests += len(
                [model for model in models if model in specific_models]
            )
        elif default_models_only:
            nb_total_tests += 1
        else:
            nb_total_tests += len(models)

    return nb_total_tests


def get_endpoints_models(url, path, header):
    response = requests.get(f"{url}{path}", headers=header)
    return response.json()["models"]


def reorder_endpoints(endpoints):
    # Reorder the enpoints in order to pass fastest test in first
    input_order = ["text", "image", "audio", "video"]
    reorder_paths = {}
    for input_order_item in input_order:
        reorder_paths.update(
            {
                key: value
                for key, value in endpoints["paths"].items()
                if key.split("/")[1] == input_order_item
            }
        )
    endpoints["paths"] = reorder_paths
    return endpoints


def request_endpoint(url, path, params={}, data={}, files={}, max_retry=3):

    tries = 1
    response = type("", (object,), {"status_code": 500})()

    while tries <= max_retry and response.status_code != 200:

        response = requests.post(
            f"{url}{path}",
            params=params,
            data=data,
            files={key: open(value[1], "rb") for key, value in files.items()},
        )

        uploaded_files = [value[0] for value in files.values()]
        url_files = [key for key in data.keys() if key.endswith("_url")]
        used_files = uploaded_files + url_files
        files_message = f" ({', '.join(used_files)})"

        print(f"|  |       ___ Try : {tries}/{max_retry}{files_message}")
        print(f"|  |      |    |_ Response : {response.status_code} ")
        tries += 1

    print(f"|  |      |")

    return response


def perform_test(
    details,
    url,
    header,
    path,
    skip_when_failed,
    max_retry=3,
    specific_models=[],
    default_models_only=False,
):
    global nb_test_ran, nb_test_passed, nb_test_failed, nb_test_skipped
    global test_final_status
    global STATUS_PASSED, STATUS_FAILED, STATUS_SKIPPED
    global nb_total_tests
    global IS_CI
    global endpoints
    global formats_to_test

    output_content_type_asserts = {
        "image/*": "image/png",
        "application/json": "application/json",
        "audio/*": "",  # TODO: define the casting content type for audio output
        "video/*": "",  # TODO: define the casting content type for video output
    }

    # Create a list a inputs to make requests needed to test the model properly
    def get_requests_inputs():
        requests_inputs = []

        if (
            "application/x-www-form-urlencoded"
            in details["post"]["requestBody"]["content"]
        ):
            request_body_info = details["post"]["requestBody"]["content"][
                "application/x-www-form-urlencoded"
            ]["schema"]
        else:
            request_body_info = details["post"]["requestBody"]["content"][
                "multipart/form-data"
            ]["schema"]

        # NOTE: never happen
        if "title" in request_body_info:
            # Simple singular input (str/int/float/bool)
            data = {request_body_info["title"]: request_body_info["default"]}
            requests_inputs.append({"data": data, "files": {}})
        else:
            # Not simple input (json of length >2, image, audio, video)
            schema = request_body_info["$ref"].split("/")[-1]
            properties = endpoints["components"]["schemas"][schema]["properties"]

            # List all kinds of inputs
            urls_files = []
            texts = []
            audios = []
            videos = []
            images = []
            for key, value in properties.items():
                if value.get("format", None) != "binary":
                    if value.get("data_type", None) == "url":
                        if "audio" in key or "audio" in value["title"].lower():
                            # URL audio
                            urls_files.append(
                                {key: "https://anshe.org/audio/3Weeks-080715.mp3"}
                            )
                        elif "video" in key or "video" in value["title"].lower():
                            # URL Video
                            urls_files.append(
                                {
                                    key: "https://upload.wikimedia.org/wikipedia/commons/8/80/Expedition_50-51_Crew_Docks_to_the_Space_Station.webm"
                                }
                            )
                        else:
                            # URL Image
                            urls_files.append(
                                {
                                    key: "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b6/Image_created_with_a_mobile_phone.png/1200px-Image_created_with_a_mobile_phone.png"
                                }
                            )
                    else:
                        texts.append({key: ("text", value["_examples"][0])})
                else:
                    if "audio" in key or "audio" in value["title"].lower():
                        # Audio
                        audios.append({key: ("audio", formats_to_test["audio"])})
                    elif "video" in key or "video" in value["title"].lower():
                        # Video
                        videos.append({key: ("video", formats_to_test["video"])})
                    else:
                        # Image
                        images.append({key: ("image", formats_to_test["image"])})

            # Create all requests to send for good testing of the model
            data = {key: value[1] for text in texts for key, value in text.items()}

            if urls_files:
                # Create a first request testing url version of files
                url_data = data.copy()
                for url_file in urls_files:
                    url_data.update(url_file)
                requests_inputs.append({"data": url_data, "files": {}})

                # Then, for each input type (audio, video, image),
                # create requests testing each file format,
                # using first one by default for the other input type
                # ex: .mp3 audio format will be tested using .jpg format for images
                all_files = [("audio", audios), ("image", images), ("video", videos)]
                for types_files in all_files:
                    # Keep only other types files (ex: 'audio' and 'video' if 'image')
                    other_types_file = [
                        item for item in all_files if item != types_files
                    ]
                    other_input_files = {}
                    # Add a file with default type format (fist one) for each other file to send
                    for other_type_file in other_types_file:
                        other_input_files = add_default_files(
                            other_input_files, other_type_file[1]
                        )
                    # Add a new set of request files for each format to test
                    for format in formats_to_test[types_files[0]]:
                        request_files = other_input_files.copy()
                        # Use the same type of format for each file of this type (ex: jpg for all images)
                        for type_file in types_files[1]:
                            # Retieve the test file in current directory with the good format ...
                            file = [
                                file
                                for file in os.listdir(CURRENT_DIRECTORY)
                                if file.endswith(format)
                            ][0]
                            file_path = os.path.join(CURRENT_DIRECTORY, file)
                            # ... and add it to the request files
                            for key, value in type_file.items():
                                request_files.update({value[0]: (file, file_path)})
                            requests_inputs.append(
                                {"data": data, "files": request_files}
                            )

            else:
                requests_inputs.append({"data": data, "files": {}})

        return requests_inputs

    def add_default_files(initial_files, files_to_add):
        """ex of files to add:
            {"Image": ("image", "[.jpg, .png]")}

        This fonction will take only the first format in the list
        and merge it with initial_files to become the new dict

        result from ex with initial_files = {}:
            {"Image": ("image", ".jpg")}
        """
        dict_to_merge = {
            key: (value[0], value[1][0])
            for files in files_to_add
            for key, value in files.items()
        }
        initial_files.update(dict_to_merge)
        return initial_files

    def is_file_in_inputs(requests_inputs):
        for request_inputs in requests_inputs:
            if request_inputs["files"]:
                return True
        return False

    requests_inputs = get_requests_inputs()

    response_output_type = list(details["post"]["responses"]["200"]["content"].keys())[
        0
    ]

    response = requests.get(f"{url}{path}", headers=header)
    if specific_models != []:
        models = [
            model for model in response.json()["models"] if model in specific_models
        ]
    elif default_models_only:
        models = [details["post"]["parameters"][0]["schema"]["default"]]
    else:
        models = response.json()["models"]

    for model in models:
        if IS_CI:
            sleep(1)

        params = {"model": model}

        valid = True
        output_type_ok = True
        for request in requests_inputs:
            response = request_endpoint(
                url=url,
                path=path,
                params=params,
                data=request["data"],
                files=request["files"],
                max_retry=max_retry,
            )

            output_type_ok = (
                output_content_type_asserts[response_output_type]
                == response.headers["content-type"]
            )

            if response.status_code != 200 or output_type_ok is False:
                valid = False

        if valid:
            nb_test_passed += 1
            status = STATUS_PASSED
        else:
            nb_test_failed += 1
            status = STATUS_FAILED
            test_final_status = EXIT_STATUS_FAILURE
            if IS_CI and not is_file_in_inputs(requests_inputs):
                sleep(2)

        nb_test_ran += 1

        progress = round((nb_test_ran / nb_total_tests) * 100, 2)
        output_type_message = (
            " ({} instead of {})".format(
                response.headers["content-type"],
                output_content_type_asserts[response_output_type],
            )
            if response.status_code == 200 and not output_type_ok
            else ""
        )
        print(
            f"|  |__ {status} {model} ({progress}%)  <{response.status_code}>{output_type_message}"
        )
        print(f"|  |")
        if skip_when_failed:
            if status == STATUS_FAILED:
                sys.exit(STATUS_FAILED)

    print("|")


def write_github_comment(github_token, github_pull_request, output):
    if github_pull_request != "main":
        print("output:" + output)
        url = f"https://api.github.com/repos/gladiaio/gladia/issues/{github_pull_request}/comments"
        header = {
            "Authorization": f"token {github_token}",
            "Content-Type": "application/x-www-form-urlencoded",
            "Accept": "application/json",
        }
        data = '{"body": "' + output.replace("\n", "\\n") + '"}'

        response = requests.post(url, headers=header, data=data)

        return response.status_code
    else:
        return "ok"


@click.command()
@click.option(
    "-u",
    "--url",
    type=str,
    default=f"http://localhost:{os.getenv('API_SERVER_PORT_HTTP', default='8080')}",
    help="URL to test",
)
@click.option(
    "-b",
    "--bearer_token",
    type=str,
    default="",
    help="Bearer token for the secured url (if applicable)",
)
@click.option(
    "-s",
    "--specific_endpoints",
    type=str,
    default="",
    help="CSV separated list of specific endpoints/routes to test format is /input/output/singular_format_task/",
)
@click.option(
    "-m",
    "--specific_models",
    type=str,
    default="",
    help="CSV separated list of specific models to test. Format is model1,model2. A model is not tested if there are specific endpoints and if the model is not in it",
)
@click.option(
    "-d",
    "--default_models",
    type=bool,
    is_flag=True,
    default=False,
    help="if default_model, only defaults models will be used, except if specific_models are selected too",
)
@click.option(
    "-c",
    "--continue_when_failed",
    type=bool,
    is_flag=True,
    default=False,
    help="Continue all other tests even when 1 test failed",
)
@click.option(
    "-a",
    "--after_endpoint",
    type=str,
    default="",
    help="All tests prior to the specified endpoint will be ignored. Specified endpoint is /input/output/singular_format_task/",
)
@click.option(
    "-r",
    "--max_retry",
    type=int,
    default=3,
    help="Number of retry for testing endpoints",
)
@click.option(
    "-g",
    "--github_comment",
    is_flag=True,
    show_default=True,
    default=False,
    help="Write the test result in a GitHub comment",
)
@click.option(
    "-t",
    "--github_token",
    type=str,
    show_default=False,
    default="",
    help="Github Token for the GitHub comment",
)
@click.option(
    "-p",
    "--github_pull_request",
    type=str,
    show_default=False,
    default="",
    help="Github Pull request for the GitHub comment",
)
@click.option(
    "--audio_file",
    type=str,
    show_default=False,
    default="",
    help="Format to test for audio file (ex: .mp3)",
)
@click.option(
    "--image_file",
    type=str,
    show_default=False,
    default="",
    help="Format to test for image file (ex: .jpg)",
)
@click.option(
    "--video_file",
    type=str,
    show_default=False,
    default="",
    help="Format to test for video file (ex: .avi)",
)
def main(
    url,
    bearer_token,
    specific_endpoints,
    specific_models,
    default_models,
    continue_when_failed,
    after_endpoint,
    max_retry,
    github_comment,
    github_token,
    github_pull_request,
    audio_file,
    image_file,
    video_file,
):

    global endpoints
    global formats_to_test

    formats_to_test = {}
    config_path = os.path.join(CURRENT_DIRECTORY, "config.json")
    with open(config_path, "r") as config_file:
        config = json.load(config_file)

        # Set audio files to use in test
        if audio_file:
            if audio_file in config["formats_to_test"]["audio"]:
                formats_to_test["audio"] = [audio_file]
            else:
                print(f"{audio_file} is not an accepted format for audio file")
                print("Try one of these ", config["formats_to_test"]["audio"])
                sys.exit(1)
        else:
            formats_to_test["audio"] = config["formats_to_test"]["audio"]

        # Set image files to use in test
        if image_file:
            if image_file in config["formats_to_test"]["image"]:
                formats_to_test["image"] = [image_file]
            else:
                print(f"{image_file} is not accepted format for image file")
                print("Try one of these ", config["formats_to_test"]["image"])
                sys.exit(1)
        else:
            formats_to_test["image"] = config["formats_to_test"]["image"]

        # Set video files to use in test
        if video_file:
            if video_file in config["formats_to_test"]["video"]:
                formats_to_test["video"] = [video_file]
            else:
                print(f"{video_file} is not accepted format for video file")
                print("Try one of these ", config["formats_to_test"]["video"])
                sys.exit(1)
        else:
            formats_to_test["video"] = config["formats_to_test"]["video"]

    skip_when_failed = not continue_when_failed
    if specific_endpoints:
        specific_endpoints = specific_endpoints.split(",")
    else:
        specific_endpoints = []

    if specific_models:
        specific_models = specific_models.split(",")
    else:
        specific_models = []

    header = {"Authorization": "Bearer " + bearer_token}
    response = requests.get(f"{url}/openapi.json", headers=header)
    endpoints = response.json()
    endpoints = reorder_endpoints(endpoints)

    # if the specific endpoint is less
    # then 4 it means it's looking to
    # have a input or input/output mod
    # this should also help to handle
    # missing trailing /
    these_specific_endpoints = []
    for specific_endpoint in specific_endpoints:
        if len(specific_endpoint.split("/")) < 4:
            for endpoint in endpoints["paths"].keys():
                if endpoint.startswith(specific_endpoint):
                    these_specific_endpoints.append(endpoint)

    if these_specific_endpoints != []:
        specific_endpoints = list(dict.fromkeys(these_specific_endpoints))

    print()
    print(f"Testing endpoints")
    print()

    global nb_total_tests, nb_test_ran, nb_test_passed, nb_test_failed, nb_test_skipped
    global test_final_status

    nb_test_skipped = 0
    nb_test_passed = 0
    nb_test_failed = 0
    nb_test_ran = 0
    nb_total_tests = get_nb_tests(
        url, header, endpoints, specific_endpoints, specific_models, default_models
    )

    after_endpoint_continue = False
    test_final_status = EXIT_STATUS_SUCCESS

    for path, details in endpoints["paths"].items():
        print(f"|__ {path}")
        print(f"|  |")
        if specific_endpoints:
            if path in specific_endpoints:
                perform_test(
                    details,
                    url,
                    header,
                    path,
                    skip_when_failed,
                    max_retry,
                    specific_models,
                    default_models,
                )

            elif after_endpoint != "":
                if path in after_endpoint:
                    after_endpoint_continue = True

                if after_endpoint_continue:
                    perform_test(
                        details,
                        url,
                        header,
                        path,
                        skip_when_failed,
                        max_retry,
                        specific_models,
                        default_models,
                    )
                else:
                    print(f"|  |__ {STATUS_SKIPPED}  <Skipped>")
                    print(f"|")
                    nb_test_skipped += 1

            else:
                print(f"|  |__ {STATUS_SKIPPED}  <Skipped>")
                print(f"|")
                nb_test_skipped += 1

        elif after_endpoint != "":
            if path in after_endpoint:
                after_endpoint_continue = True

            if after_endpoint_continue:
                perform_test(
                    details,
                    url,
                    header,
                    path,
                    skip_when_failed,
                    max_retry,
                    specific_models,
                    default_models,
                )
            else:
                print(f"|  |__ {STATUS_SKIPPED}  <Skipped>")
                print(f"|")
                nb_test_skipped += 1
        else:
            perform_test(
                details,
                url,
                header,
                path,
                skip_when_failed,
                max_retry,
                specific_models,
                default_models,
            )

    if test_final_status == EXIT_STATUS_SUCCESS:
        str_final_status = "Success"
    else:
        str_final_status = "Failure"
    output = f"""
    Final status: {str_final_status}
    Test Passed: {nb_test_passed}/{nb_total_tests} ({round((nb_test_passed / nb_total_tests)*100, 2)}%)
    Test Failed: {nb_test_failed}/{nb_total_tests}  ({round((nb_test_failed / nb_total_tests)*100, 2)}%)
    Test Skipped: {nb_test_skipped}
    """

    print(output)

    if github_comment:
        if github_token == "":
            print("Github token is required to write a comment")
            sys.exit(1)
        if github_pull_request == "":
            print("Github pull request is required to write a comment")
            sys.exit(1)
        write_github_comment(github_token, github_pull_request, output)

    sys.exit(test_final_status)


if __name__ == "__main__":
    global IS_CI
    IS_CI = os.getenv("IS_CI", "False").lower() in ("true", "1", "t", "y", "yes")
    main()
