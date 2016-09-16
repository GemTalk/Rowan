"
notice: Copyright 2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
classStructureFrom: fileteeClassPropertiesDict comment: classComment
  | classPropertiesDict subclassType filetreeSubclassType |
  classPropertiesDict := fileteeClassPropertiesDict copy.
  filetreeSubclassType := classPropertiesDict at: 'type'.
  filetreeSubclassType = 'normal'
    ifTrue: [ subclassType := '' ]
    ifFalse: [ 
      filetreeSubclassType = 'variable'
        ifTrue: [ subclassType := 'indexableSubclass' ]
        ifFalse: [ 
          filetreeSubclassType = 'bytes'
            ifTrue: [ subclassType := 'byteSubclass' ]
            ifFalse: [ self error: 'unknown subclass type: ' , filetreeSubclassType printString ] ] ].
  classPropertiesDict at: '_gs_subclassType' put: subclassType.
  ^ super classStructureFrom: classPropertiesDict comment: classComment
