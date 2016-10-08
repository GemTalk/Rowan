"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
currentBranch
  | result |
  result := self
    gitCommand: #('rev-parse' '--abbrev-ref' 'HEAD')
    in: self directoryPath.
  ^ result trimBlanks