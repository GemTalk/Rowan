"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: printing
"
printDetailsOn: aStream
  self alias notEmpty
    ifTrue: [ aStream
        nextPutAll: self alias;
        nextPutAll: ': ' ].
  aStream nextPutAll: self url printString
