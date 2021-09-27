import torch

from transformers import AutoTokenizer, AutoModelForSeq2SeqLM


def predict(text):
    barthez_tokenizer = AutoTokenizer.from_pretrained("moussaKam/barthez")
    barthez_model = AutoModelForSeq2SeqLM.from_pretrained("moussaKam/barthez-orangesum-abstract")


    input_ids = torch.tensor(
        [barthez_tokenizer.encode(text, add_special_tokens=True)]
    )

    predict = barthez_model.forward(input_ids)[0]
    print(predict)
    return ("positive" if predict.argmax(dim=-1).item()==1 else "negative") 
    