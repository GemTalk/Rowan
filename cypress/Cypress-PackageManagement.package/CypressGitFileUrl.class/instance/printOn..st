"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: as yet unclassified
"
printOn: aStream
  | escapedBranchOrTag |
  aStream nextPutAll: self schemeName , '://' , self host.
  aStream
    nextPutAll: self projectPath;
    nextPut: $:.
  escapedBranchOrTag := String
    streamContents: [ :stream | self projectBranchOrTag do: [ :char | char = $/
            ifTrue: [ stream nextPut: $\ ].
          stream nextPut: char ] ].
  aStream
    nextPutAll: escapedBranchOrTag;
    nextPut: $/;
    nextPutAll: self repositoryPath;
    yourself