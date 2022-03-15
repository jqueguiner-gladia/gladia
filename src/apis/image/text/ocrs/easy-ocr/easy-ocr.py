import easyocr


def predict(image, source_language):

    reader = easyocr.Reader([source_language], gp=True)
    text = reader.readtext(image, detail=0)
    
    return text