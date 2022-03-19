import torch

from transformers import AutoTokenizer, AutoModelForQuestionAnswering, PreTrainedTokenizer, PreTrainedModel


def load_model() -> (PreTrainedTokenizer, PreTrainedModel):
    model_name = "deepset/bert-base-cased-squad2"

    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model = AutoModelForQuestionAnswering.from_pretrained(model_name)

    return tokenizer, model
    

def predict(context, question):

    tokenizer, model = load_model()

    inputs = tokenizer.encode_plus(question, context, add_special_tokens=True, return_tensors="pt")
    input_ids = inputs["input_ids"].tolist()[0]

    text_tokens = tokenizer.convert_ids_to_tokens(input_ids)
    answer_start_scores, answer_end_scores = model(**inputs, return_dict=False)

    answer_start = torch.argmax(answer_start_scores)
    answer_end = torch.argmax(answer_end_scores) + 1

    answer = tokenizer.convert_tokens_to_string(tokenizer.convert_ids_to_tokens(input_ids[answer_start:answer_end]))

    return answer
