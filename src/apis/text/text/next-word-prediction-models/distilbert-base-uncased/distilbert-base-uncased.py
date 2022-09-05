from typing import Dict, Tuple, Union, Optional, List
from transformers import FillMaskPipeline, AutoModelForMaskedLM, DistilBertTokenizerFast

def predict_mask(pipeline, text: str, targets: Optional[List[str]] = None, top_k: int = 1) -> List[Tuple[str, float]]:
    """
    Predict [MASK] tokens in a string.
    targets limit possible guesses if supplied.
    top_k describes number of targets to return*
    *top_k does not apply if targets is supplied
    """

    answers = pipeline(
        text,
        targets=targets, top_k=top_k
    )

    return [
        (
            answer["token_str"],
            answer["score"]
        ) for answer in answers
    ]


def predict(sentence: str, top_k: int = 3) -> Dict[str, Union[str, Dict[str, float]]]:
    """
    For a given sentence, predict the next word.

    Args:
        sentence (str): The sentence to predict the next word from.

    Returns:
        Dict[str, Union[str, Dict[str, float]]]: The next word predicted and score from the sentence.
    """

    model_checkpoint = "distilbert-base-uncased"

    pipeline = FillMaskPipeline(
        model=AutoModelForMaskedLM.from_pretrained(model_checkpoint),
        tokenizer=DistilBertTokenizerFast.from_pretrained(model_checkpoint),
    )

    predictions = predict_mask(pipeline, f"{sentence} [MASK]", top_k=top_k)

    return {
        "prediction": predictions[0][0],
        "prediction_raw": predictions,
    }
