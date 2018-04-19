Extension { #name : 'PositionableStreamPortable' }

{ #category : '*tonel-gemstonecommon-core' }
PositionableStreamPortable >> match: subCollection [
  "Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

  | pattern startMatch |
  pattern := ReadStreamPortable on: subCollection.
  startMatch := nil.
  [ pattern atEnd ]
    whileFalse: [ 
      self atEnd
        ifTrue: [ ^ false ].
      self next = pattern next
        ifTrue: [ 
          pattern position = 1
            ifTrue: [ startMatch := self position ] ]
        ifFalse: [ 
          pattern position: 0.
          startMatch
            ifNotNil: [ 
              self position: startMatch.
              startMatch := nil ] ] ].
  ^ true

]

{ #category : '*tonel-gemstonecommon-core' }
PositionableStreamPortable >> originalContents [
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection

]
