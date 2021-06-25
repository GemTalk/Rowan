Extension { #name : 'CharacterCollection' }

{ #category : '*ston-gemstonecommon' }
CharacterCollection >> stonContainSubObjects [
  ^ false

]

{ #category : '*ston-gemstonecommon' }
CharacterCollection >> stonOn: stonWriter [

        self isSymbol
                ifTrue: [stonWriter writeSymbol: self]
                ifFalse: [stonWriter writeString: self]

]

{ #category : '*ston-gemstonecommon' }
CharacterCollection class >> findFirstInString: aString inSet: inclusionMap startingAt: start [

	"Trivial, non-primitive version"

	| i stringSize ascii |
	inclusionMap size ~= 256
		ifTrue: [ ^ 0 ].

	i := start.
	stringSize := aString size.
	[ 
	i <= stringSize
		and: [ 
			ascii := (aString at: i) asciiValue.
			ascii < 256
				ifTrue: [ (inclusionMap at: ascii + 1) = 0 ]
				ifFalse: [ true ] ] ]
		whileTrue: [ i := i + 1 ].

	i > stringSize
		ifTrue: [ ^ 0 ].
	^ i

]
