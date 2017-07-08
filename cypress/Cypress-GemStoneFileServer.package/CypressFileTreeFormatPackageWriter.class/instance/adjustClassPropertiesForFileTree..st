"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
adjustClassPropertiesForFileTree: classPropertyDict
  | props classType |
  props := classPropertyDict copy.
  classType := (props at: '_gs_subclassType' ifAbsent: [  ])
    ifNil: [ 'normal' ]
    ifNotNil: [ :type | 
      props removeKey: '_gs_subclassType'.
      type = 'indexableSubclass'
        ifTrue: [ 'variable' ]
        ifFalse: [ 
          type = 'byteSubclass'
            ifTrue: [ 'bytes' ]
            ifFalse: [ self error: 'unknown subclass type: ' , type ] ] ].
  props at: 'type' put: classType.
  ^ props
