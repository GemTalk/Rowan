"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
repositoryPath: aString
  (aString size > 0
    and: [ aString last = self fileUtils pathNameDelimiter last ])
    ifTrue: [ repositoryPath := aString copyFrom: 1 to: aString size - 1 ]
    ifFalse: [ repositoryPath := aString ]
