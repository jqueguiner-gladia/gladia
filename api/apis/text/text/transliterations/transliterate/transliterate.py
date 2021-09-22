from transliterate import translit, get_available_language_codes

def predict(text, language):
    return translit(text, language)
    
