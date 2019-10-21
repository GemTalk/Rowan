Extension { #name : 'RowanService' }

{ #category : 'perform' }
RowanService >> servicePerform: symbol withArguments: collection [
  "each service updates itself after performing a command.
	Therefore, if the command is #update, don't run it here"

  SessionTemps current
    at: #'versionsVerified'
    ifAbsent: [ 
      Error
        signal:
          'Version mismatch failure. Client version is older than server version.'.
      System logout ].
  symbol == #'update'
    ifTrue: [ ^ self ].
  ^ super perform: symbol withArguments: collection
]
