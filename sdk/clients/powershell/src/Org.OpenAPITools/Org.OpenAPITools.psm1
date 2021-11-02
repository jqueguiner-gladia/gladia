#region Import functions

'API', 'Model', 'Private' | Get-ChildItem -Path {
    Join-Path $PSScriptRoot $_
} -Filter '*.ps1' | ForEach-Object {
    Write-Verbose "Importing file: $($_.BaseName)"
    try {
        . $_.FullName
    } catch {
        Write-Verbose "Can't import function!"
    }
}

#endregion


#region Initialize APIs

'Creating object: Org.OpenAPITools.Api.ImageImageBackgroundRemovalApi' | Write-Verbose
$Script:ImageImageBackgroundRemovalApi= New-Object -TypeName Org.OpenAPITools.Api.ImageImageBackgroundRemovalApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.ImageImageColorizationApi' | Write-Verbose
$Script:ImageImageColorizationApi= New-Object -TypeName Org.OpenAPITools.Api.ImageImageColorizationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.ImageImageFaceBluringApi' | Write-Verbose
$Script:ImageImageFaceBluringApi= New-Object -TypeName Org.OpenAPITools.Api.ImageImageFaceBluringApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.ImageImageRestorationApi' | Write-Verbose
$Script:ImageImageRestorationApi= New-Object -TypeName Org.OpenAPITools.Api.ImageImageRestorationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.ImageImageSuperResolutionApi' | Write-Verbose
$Script:ImageImageSuperResolutionApi= New-Object -TypeName Org.OpenAPITools.Api.ImageImageSuperResolutionApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.ImageImageUncolorizationApi' | Write-Verbose
$Script:ImageImageUncolorizationApi= New-Object -TypeName Org.OpenAPITools.Api.ImageImageUncolorizationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextAutocorrectApi' | Write-Verbose
$Script:TextTextAutocorrectApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextAutocorrectApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextBooleanQuestionGenerationApi' | Write-Verbose
$Script:TextTextBooleanQuestionGenerationApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextBooleanQuestionGenerationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextDependencyTrackingApi' | Write-Verbose
$Script:TextTextDependencyTrackingApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextDependencyTrackingApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextEmotionRecognitionApi' | Write-Verbose
$Script:TextTextEmotionRecognitionApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextEmotionRecognitionApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextEntityExtractionApi' | Write-Verbose
$Script:TextTextEntityExtractionApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextEntityExtractionApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextHateSpeechDetectionApi' | Write-Verbose
$Script:TextTextHateSpeechDetectionApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextHateSpeechDetectionApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextKeywordExtractionApi' | Write-Verbose
$Script:TextTextKeywordExtractionApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextKeywordExtractionApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextLanguageDetectionApi' | Write-Verbose
$Script:TextTextLanguageDetectionApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextLanguageDetectionApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextLanguageGenerationApi' | Write-Verbose
$Script:TextTextLanguageGenerationApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextLanguageGenerationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextLemmatizationApi' | Write-Verbose
$Script:TextTextLemmatizationApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextLemmatizationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextNamedEntityRecognitionApi' | Write-Verbose
$Script:TextTextNamedEntityRecognitionApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextNamedEntityRecognitionApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextNextSentencePredictionApi' | Write-Verbose
$Script:TextTextNextSentencePredictionApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextNextSentencePredictionApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextNextWordPredictionApi' | Write-Verbose
$Script:TextTextNextWordPredictionApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextNextWordPredictionApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextPluralApi' | Write-Verbose
$Script:TextTextPluralApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextPluralApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextProgrammingLanguageGenerationApi' | Write-Verbose
$Script:TextTextProgrammingLanguageGenerationApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextProgrammingLanguageGenerationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextProgrammingLanguageIdentificationApi' | Write-Verbose
$Script:TextTextProgrammingLanguageIdentificationApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextProgrammingLanguageIdentificationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextQuestionAnsweringApi' | Write-Verbose
$Script:TextTextQuestionAnsweringApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextQuestionAnsweringApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextSentencePairModelingApi' | Write-Verbose
$Script:TextTextSentencePairModelingApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextSentencePairModelingApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextSentenceParaphraserApi' | Write-Verbose
$Script:TextTextSentenceParaphraserApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextSentenceParaphraserApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextSentimentAnalysisApi' | Write-Verbose
$Script:TextTextSentimentAnalysisApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextSentimentAnalysisApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextSimilarityApi' | Write-Verbose
$Script:TextTextSimilarityApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextSimilarityApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextSummarizationApi' | Write-Verbose
$Script:TextTextSummarizationApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextSummarizationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextTranslationApi' | Write-Verbose
$Script:TextTextTranslationApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextTranslationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextTransliterationApi' | Write-Verbose
$Script:TextTextTransliterationApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextTransliterationApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.TextTextWordAlignmentApi' | Write-Verbose
$Script:TextTextWordAlignmentApi= New-Object -TypeName Org.OpenAPITools.Api.TextTextWordAlignmentApi -ArgumentList @($null)


#endregion
