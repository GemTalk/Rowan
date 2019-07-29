"
I am ZnCharacterEncodingError.
I am an Error.

I signal when something goes wrong while encoding or decoding characters.

Part of Zinc HTTP Components
"
Class {
	#name : 'ZnCharacterEncodingError',
	#superclass : 'Error',
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : 'private' }
ZnCharacterEncodingError >> defaultResumeValue [
	"Answer the value that by default should be returned if the exception is resumed"
	
	^ nil
]

{ #category : 'Handling' }
ZnCharacterEncodingError >> resume [

"See  resume:  for documentation ."

 self isResumable ifFalse:[ 
   "cannot resume from a not-resumable Exception."
   ^ self error:'cannot resume from a not-resumable Exception'
 ].
 ^ self _resume: self defaultResumeValue
]
