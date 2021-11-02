QT += network

HEADERS += \
# Models
    $${PWD}/OAIBody_apply_image_image_background_removal__post.h \
    $${PWD}/OAIBody_apply_image_image_colorization__post.h \
    $${PWD}/OAIBody_apply_image_image_face_bluring__post.h \
    $${PWD}/OAIBody_apply_image_image_restoration__post.h \
    $${PWD}/OAIBody_apply_image_image_super_resolution__post.h \
    $${PWD}/OAIBody_apply_image_image_uncolorization__post.h \
    $${PWD}/OAIHTTPValidationError.h \
    $${PWD}/OAIValidationError.h \
# APIs
    $${PWD}/OAIImageImageBackgroundRemovalApi.h \
    $${PWD}/OAIImageImageColorizationApi.h \
    $${PWD}/OAIImageImageFaceBluringApi.h \
    $${PWD}/OAIImageImageRestorationApi.h \
    $${PWD}/OAIImageImageSuperResolutionApi.h \
    $${PWD}/OAIImageImageUncolorizationApi.h \
    $${PWD}/OAITextTextAutocorrectApi.h \
    $${PWD}/OAITextTextBooleanQuestionGenerationApi.h \
    $${PWD}/OAITextTextDependencyTrackingApi.h \
    $${PWD}/OAITextTextEmotionRecognitionApi.h \
    $${PWD}/OAITextTextEntityExtractionApi.h \
    $${PWD}/OAITextTextHateSpeechDetectionApi.h \
    $${PWD}/OAITextTextKeywordExtractionApi.h \
    $${PWD}/OAITextTextLanguageDetectionApi.h \
    $${PWD}/OAITextTextLanguageGenerationApi.h \
    $${PWD}/OAITextTextLemmatizationApi.h \
    $${PWD}/OAITextTextNamedEntityRecognitionApi.h \
    $${PWD}/OAITextTextNextSentencePredictionApi.h \
    $${PWD}/OAITextTextNextWordPredictionApi.h \
    $${PWD}/OAITextTextPluralApi.h \
    $${PWD}/OAITextTextProgrammingLanguageGenerationApi.h \
    $${PWD}/OAITextTextProgrammingLanguageIdentificationApi.h \
    $${PWD}/OAITextTextQuestionAnsweringApi.h \
    $${PWD}/OAITextTextSentencePairModelingApi.h \
    $${PWD}/OAITextTextSentenceParaphraserApi.h \
    $${PWD}/OAITextTextSentimentAnalysisApi.h \
    $${PWD}/OAITextTextSimilarityApi.h \
    $${PWD}/OAITextTextSummarizationApi.h \
    $${PWD}/OAITextTextTranslationApi.h \
    $${PWD}/OAITextTextTransliterationApi.h \
    $${PWD}/OAITextTextWordAlignmentApi.h \
# Others
    $${PWD}/OAIHelpers.h \
    $${PWD}/OAIHttpRequest.h \
    $${PWD}/OAIObject.h
    $${PWD}/OAIEnum.h    

SOURCES += \
# Models
    $${PWD}/OAIBody_apply_image_image_background_removal__post.cpp \
    $${PWD}/OAIBody_apply_image_image_colorization__post.cpp \
    $${PWD}/OAIBody_apply_image_image_face_bluring__post.cpp \
    $${PWD}/OAIBody_apply_image_image_restoration__post.cpp \
    $${PWD}/OAIBody_apply_image_image_super_resolution__post.cpp \
    $${PWD}/OAIBody_apply_image_image_uncolorization__post.cpp \
    $${PWD}/OAIHTTPValidationError.cpp \
    $${PWD}/OAIValidationError.cpp \
# APIs
    $${PWD}/OAIImageImageBackgroundRemovalApi.cpp \
    $${PWD}/OAIImageImageColorizationApi.cpp \
    $${PWD}/OAIImageImageFaceBluringApi.cpp \
    $${PWD}/OAIImageImageRestorationApi.cpp \
    $${PWD}/OAIImageImageSuperResolutionApi.cpp \
    $${PWD}/OAIImageImageUncolorizationApi.cpp \
    $${PWD}/OAITextTextAutocorrectApi.cpp \
    $${PWD}/OAITextTextBooleanQuestionGenerationApi.cpp \
    $${PWD}/OAITextTextDependencyTrackingApi.cpp \
    $${PWD}/OAITextTextEmotionRecognitionApi.cpp \
    $${PWD}/OAITextTextEntityExtractionApi.cpp \
    $${PWD}/OAITextTextHateSpeechDetectionApi.cpp \
    $${PWD}/OAITextTextKeywordExtractionApi.cpp \
    $${PWD}/OAITextTextLanguageDetectionApi.cpp \
    $${PWD}/OAITextTextLanguageGenerationApi.cpp \
    $${PWD}/OAITextTextLemmatizationApi.cpp \
    $${PWD}/OAITextTextNamedEntityRecognitionApi.cpp \
    $${PWD}/OAITextTextNextSentencePredictionApi.cpp \
    $${PWD}/OAITextTextNextWordPredictionApi.cpp \
    $${PWD}/OAITextTextPluralApi.cpp \
    $${PWD}/OAITextTextProgrammingLanguageGenerationApi.cpp \
    $${PWD}/OAITextTextProgrammingLanguageIdentificationApi.cpp \
    $${PWD}/OAITextTextQuestionAnsweringApi.cpp \
    $${PWD}/OAITextTextSentencePairModelingApi.cpp \
    $${PWD}/OAITextTextSentenceParaphraserApi.cpp \
    $${PWD}/OAITextTextSentimentAnalysisApi.cpp \
    $${PWD}/OAITextTextSimilarityApi.cpp \
    $${PWD}/OAITextTextSummarizationApi.cpp \
    $${PWD}/OAITextTextTranslationApi.cpp \
    $${PWD}/OAITextTextTransliterationApi.cpp \
    $${PWD}/OAITextTextWordAlignmentApi.cpp \
# Others
    $${PWD}/OAIHelpers.cpp \
    $${PWD}/OAIHttpRequest.cpp

