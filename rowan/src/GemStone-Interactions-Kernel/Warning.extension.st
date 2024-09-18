Extension { #name : 'Warning' }

{ #category : '*gemstone-interactions-kernel' }
Warning >> defaultAction [
  "The user should be notified of the occurrence of an exceptional
	occurrence and given an option of continuing or aborting the
	computation. The description of the occurrence should include
	any text specified as the argument of the #signal: message."

  | response |
  GsNotifyInteraction interactionsEnabled ifFalse: [ ^ super defaultAction ].
  response := (GsNotifyInteraction
    prompt:
      self description withoutGemstoneLineEndings , ' Press ''Yes'' to continue.'
    confirm: 'Proceed'
    cancel: 'Debug'
    abort: 'Cancel') signal.
  response == true
    ifTrue: [ ^ super defaultAction ].
  response == false
    ifTrue: [ self halt: 'Debugging: ' , self description ].
  ^ Processor activeProcess terminate
]
