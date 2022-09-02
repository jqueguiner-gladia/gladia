from typing import Dict, List, Union

import truecase
from happytransformer import HappyQuestionAnswering


def predict(
    context: str, question: str
) -> Dict[str, Union[str, List[Dict[str, Union[str, float, int]]]]]:
    """
    Using the given `context`, answer the provided `question`.

    Args:
        context (str): The context to use for answering the question.
        question (str): The question to answer.

    Returns:
        Dict[str, Union[str, List[Dict[str, Union[str, float, int]]]]]: The answer to the question, the associated score and the start and end position of the answer.
    """

    NB_RESULTS = 25

    happy_qa = HappyQuestionAnswering("BERT", "mrm8488/bert-tiny-5-finetuned-squadv2")
    result = happy_qa.answer_question(
        truecase.get_true_case(context),
        truecase.get_true_case(question),
        top_k=NB_RESULTS,
    )

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
