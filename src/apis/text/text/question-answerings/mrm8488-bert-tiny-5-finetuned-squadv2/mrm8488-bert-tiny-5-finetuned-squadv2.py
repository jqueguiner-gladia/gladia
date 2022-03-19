import json

from happytransformer import HappyQuestionAnswering


def predict(context, question):
    
    happy_qa = HappyQuestionAnswering("BERT", "mrm8488/bert-tiny-5-finetuned-squadv2")
    result = happy_qa.answer_question(context, question)

    return json.dumps({'answer': result[0].answer, 'score': result[0].score})
