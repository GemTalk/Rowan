Extension { #name : 'PositionableStreamPortable' }

{ #category : '*filesystem-gemstone-kernel' }
PositionableStreamPortable >> collectionSpecies [

	^self collection species

]

{ #category : '*filesystem-gemstone-kernel' }
PositionableStreamPortable >> isBinary [
	"Return true if the receiver is a binary byte stream"
	^collection class == ByteArray

]

{ #category : '*filesystem-gemstone-kernel' }
PositionableStreamPortable >> nextInto: aCollection [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.
]

{ #category : '*filesystem-gemstone-kernel' }
PositionableStreamPortable >> nextMatchAll: aColl [
    "Answer true if next N objects are the ones in aColl,
     else false.  Advance stream of true, leave as was if false."
    | save |
    save := self position.
    aColl do: [:each |
       (self next) = each ifFalse: [
            self position: save.
            ^ false]
        ].
    ^ true
]

{ #category : '*filesystem-gemstone-kernel' }
PositionableStreamPortable >> originalContents [
	"Answer the receiver's actual contents collection, NOT a copy."

	^ collection

]

{ #category : '*filesystem-gemstone-kernel' }
PositionableStreamPortable >> setFrom: newStart to: newStop [

	position := newStart - 1.
	readLimit := newStop
]
