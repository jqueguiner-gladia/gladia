from transformers import AutoTokenizer, AutoModelForSeq2SeqLM


def predict(text):

    model_name = "mrm8488/t5-base-finetuned-emotion"

    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model = AutoModelForSeq2SeqLM.from_pretrained(model_name)

    input_ids = tokenizer.encode(text, return_tensors='pt')
    
    outputs = model.generate(input_ids)
    
    decoded = tokenizer.decode(outputs[0], skip_special_tokens=True, clean_up_tokenization_spaces=True)

    return decoded
