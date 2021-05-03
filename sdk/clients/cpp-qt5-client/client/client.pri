QT += network

HEADERS += \
# Models
    $${PWD}/OAIHTTPValidationError.h \
    $${PWD}/OAIValidationError.h \
# APIs
    $${PWD}/OAIDefaultApi.h \
    $${PWD}/OAIUsApi.h \
    $${PWD}/OAIUsersApi.h \
# Others
    $${PWD}/OAIHelpers.h \
    $${PWD}/OAIHttpRequest.h \
    $${PWD}/OAIObject.h
    $${PWD}/OAIEnum.h    

SOURCES += \
# Models
    $${PWD}/OAIHTTPValidationError.cpp \
    $${PWD}/OAIValidationError.cpp \
# APIs
    $${PWD}/OAIDefaultApi.cpp \
    $${PWD}/OAIUsApi.cpp \
    $${PWD}/OAIUsersApi.cpp \
# Others
    $${PWD}/OAIHelpers.cpp \
    $${PWD}/OAIHttpRequest.cpp

