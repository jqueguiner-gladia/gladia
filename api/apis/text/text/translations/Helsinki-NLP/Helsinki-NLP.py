import os
import pickle
import time
from pathlib import Path
from typing import List

import redis
from ai_api_utils.model_management import download_model
from ai_api_utils.system import path_to_absolute
from icecream import ic
from transformers import MarianMTModel, MarianTokenizer


class Translator:
    def __init__(self, models_dir):
        self.models_dir = models_dir
        self.model_dir = path_to_absolute(f"{self.models_dir}/opus-mt-")
        self.model = None
        self.tokenizer = None

    def load_model(self, source, target):
        route = f"{source}-{target}"

        # try:
        if True:
            if not os.path.exists(self.model_dir):

                self.model_dir = self.download_translation_models(source, target)

            ic("Loading model")

            if not os.path.exists(f"/media/virtuelram2/{route}/model"):
                Path(f"/media/virtuelram2/{route}").mkdir(parents=True, exist_ok=True)
                self.model = MarianMTModel.from_pretrained(self.model_dir)
                with open(f"/media/virtuelram2/{route}/model", "wb") as f:
                    pickle.dump(self.model, f)
            else:
                with open(f"/media/virtuelram2/{route}/model", "rb") as file:
                    self.model = pickle.loads(file.read())

            if not os.path.exists(f"/media/virtuelram2/{route}/tokenizer"):
                Path(f"/media/virtuelram2/{route}").mkdir(parents=True, exist_ok=True)
                self.tokenizer = MarianTokenizer.from_pretrained(self.model_dir)
                with open(f"/media/virtuelram2/{route}/tokenizer", "wb") as f:
                    pickle.dump(self.tokenizer, f)
            else:
                with open(f"/media/virtuelram2/{route}/tokenizer", "rb") as file:
                    self.tokenizer = pickle.loads(file.read())

            ic("Done Loading")

            return 1, f"Successfully loaded model for {route} transation"
        # except:
        #    return 0, f"Error while loading model for {route} transaction"

    def translate(self, source, target, src_text):
        route = f"{source}-{target}"

        self.model_dir += route

        success_code, message = self.load_model(source, target)
        if not success_code:
            return message

        translated = self.model.generate(
            **self.tokenizer(src_text, return_tensors="pt", padding=True)
        )
        words = [self.tokenizer.decode(t, skip_special_tokens=True) for t in translated]
        return words

    def download_translation_models(self, source, target):
        output_path = download_model(
            f"https://huggingface.co/Helsinki-NLP/opus-mt-{source}-{target}",
            self.model_dir,
        )
        ic(output_path)
        return output_path


def predict(input_string, source_language, target_language):

    translator = Translator("models")

    translation = translator.translate(source_language, target_language, input_string)
    return translation
