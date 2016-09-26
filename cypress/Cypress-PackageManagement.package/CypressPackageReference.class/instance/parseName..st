"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
parseName: aString
  | basicName index |
  basicName := aString.
  index := basicName indexOfSubCollection: '.' startingAt: 1.
  index = 0
    ifTrue: [ 
      package := basicName.
      branch := '' ]
    ifFalse: [ 
      package := basicName copyFrom: 1 to: index - 1.
      branch := basicName copyFrom: index to: basicName size ]