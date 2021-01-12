Extension { #name : 'RowanMethodService' }

{ #category : '*rowan-services-core-37x' }
RowanMethodService >> _initializeBreakPointsFor: theMethod [
  "Answers an Array stepPoints - _allBreakpoints array size changed in 3.7.0"
  | list |
  list := OrderedCollection new.
  theMethod _allBreakpoints
    ifNil: [ ^ OrderedCollection new ]
    ifNotNil: [ :anArray | 
      1 to: anArray size by: 4 do: [ :i | 
        list
          add:
            (theMethod _stepPointForMeth: (anArray at: i + 1) ip: (anArray at: i + 2)) ] ].
  ^ list asOrderedCollection
]
