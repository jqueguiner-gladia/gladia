from typing import Dict, List, Union

from happytransformer import HappyQuestionAnswering


def predict(
    context: str, question: str
) -> Dict[str, Union[str, List[Dict[str, Union[str, float, int]]]]]:
    """
    Using the given `context`, answer the provided `question`.

    :param context: context to use to answer the question
    :param question: question to answer
    :return: JSON formatted str containing both the answer and the confidence score.
    """

    NB_RESULTS = 25

    happy_qa = HappyQuestionAnswering("BERT", "mrm8488/bert-tiny-5-finetuned-squadv2")
    result = happy_qa.answer_question(context, question, top_k=NB_RESULTS)

    prediction_raw = [
        {
            "answer": answer.answer,
            "score": answer.score,
            "start": answer.start,
            "end": answer.end,
        }
        for answer in result
    ]
    prediction = result[0].answer

    del happy_qa

    return {"prediction": prediction, "prediction_raw": prediction_raw}
