Extension { #name : 'Warning' }

{ #category : '*gemstone-interactionskernel' }
Warning >> defaultAction [
  "The user should be notified of the occurrence of an exceptional
	occurrence and given an option of continuing or aborting the
	computation. The description of the occurrence should include
	any text specified as the argument of the #signal: message."

  | response |
  response := (GsNotifyInteraction
    prompt:
      self description withoutGemstoneLineEndings , ' Press ''Proceed'' to continue.'
    confirm: 'Proceed'
    cancel: 'Debug'
    abort: 'Cancel') signal.
  response == true
    ifTrue: [ ^ super defaultAction ].
  response == false
    ifTrue: [ self halt: 'Debugging: ' , self description ].
  ^ Processor activeProcess terminate
]
