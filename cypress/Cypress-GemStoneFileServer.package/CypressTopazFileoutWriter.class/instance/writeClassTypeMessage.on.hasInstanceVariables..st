"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
writeClassTypeMessage: classStructure on: aStream hasInstanceVariables: instanceVariableBlock
  | classType classTypeMessage hasInstanceVariables |
  hasInstanceVariables := true.
  classType := classStructure subclassType.
  classType = 'indexableSubclass'
    ifTrue: [ classTypeMessage := 'indexableSubclass: ' ]
    ifFalse: [ classType = 'byteSubclass'
        ifTrue: [ classTypeMessage := 'byteSubclass: '.
          hasInstanceVariables := false ]
        ifFalse: [ classType = ''
            ifTrue: [ classTypeMessage := 'subclass: ' ]
            ifFalse: [ self error: 'unknown subclass type: ' , classType ] ] ].
  aStream
    tab;
    nextPutAll: classTypeMessage , classStructure className asString printString;
    lf.
  hasInstanceVariables
    ifTrue: [ instanceVariableBlock value ]