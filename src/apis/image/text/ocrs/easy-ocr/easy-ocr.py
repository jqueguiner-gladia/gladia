import easyocr


def predict(image, source_language):

    reader = easyocr.Reader([source_language], gpu=True)
    text = reader.readtext(image, detail=False)
    
    return text
