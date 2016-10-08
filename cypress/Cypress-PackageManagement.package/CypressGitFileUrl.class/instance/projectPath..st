"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
projectPath: aString
  aString last = self fileUtils pathNameDelimiter last
    ifTrue: [ projectPath := aString copyFrom: 1 to: aString size - 1 ]
    ifFalse: [ projectPath := aString ]