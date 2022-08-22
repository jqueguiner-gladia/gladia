import os

def replace_files():
    root_path = os.getcwd()
    path = os.path.join(root_path, "apis/image/text/classification-models")
    model_dir_list = os.listdir(path)
    for model_dir in model_dir_list:
        model_dir_path = os.path.join(path, model_dir)
        if os.path.isdir(model_dir_path):
            model_file_list = os.listdir(model_dir_path)
            for model_file in model_file_list:
                if model_file.endswith(".py") and model_file != '__init__.py':
                    model_path = os.path.join(model_dir_path, model_file)
                    search_txt = '{ "prediction": output["prediction"], "prediction_raw": output["prediction_raw"] }'
                    replace_txt = 'return { "prediction": output["prediction"], "prediction_raw": output["prediction_raw"] }'
                    replace_text(model_path, search_txt, replace_txt)
                    search_txt = '[str]'
                    replace_txt = 'dict'
                    replace_text(model_path, search_txt, replace_txt)

def replace_text(path, search_txt, replace_txt):
    with open(path, 'r') as file:
        data = file.read()
        data = data.replace(search_txt, replace_txt)
    with open(path, 'w') as file:
        file.write(data)

if __name__ == "__main__":
    replace_files()

    