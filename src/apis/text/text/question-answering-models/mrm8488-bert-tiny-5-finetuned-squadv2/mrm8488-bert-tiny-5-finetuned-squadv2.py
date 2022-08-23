import json


def predict(context: str, question: str) -> dict:
    """
    Using the given `context`, answer the provided `question`.

    :param context: context to use to answer the question
    :param question: question to answer
    :return: JSON formatted str containing both the answer and the confidence score.
    """

    from happytransformer import HappyQuestionAnswering

    happy_qa = HappyQuestionAnswering("BERT", "mrm8488/bert-tiny-5-finetuned-squadv2")
    result = happy_qa.answer_question(context, question, top_k=25)

    prediction_raw = [
        {
            "answer": answer.answer, 
            "score": answer.score, 
            "start":answer.start, 
            "end": answer.end
        } for answer in result
    ]
    prediction = result[0].answer

    del happy_qa

    return {"prediction": prediction, "prediction_raw": prediction_raw}
    
