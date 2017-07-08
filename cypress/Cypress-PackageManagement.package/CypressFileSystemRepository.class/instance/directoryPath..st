"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
directoryPath: aString
  | delim |
  delim := self fileUtils pathNameDelimiter.
  aString last = delim last
    ifTrue: [ directoryPath := aString ]
    ifFalse: [ directoryPath := aString , delim ]
