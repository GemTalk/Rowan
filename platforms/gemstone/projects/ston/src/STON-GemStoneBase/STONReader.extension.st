Extension { #name : 'STONReader' }

{ #category : '*ston-gemstonebase' }
STONReader >> lookupClass: name [
  ^ (System myUserProfile objectNamed: name asSymbol)
    ifNil: [ 
		(((AllUsers userWithId: 'SystemUser') objectNamed: 'RowanTools')
			ifNotNil: [:rowanSymbolDictionary |
				(rowanSymbolDictionary at: name asSymbol ifAbsent: [])
					ifNotNil: [:cls | ^cls ] ])
						ifNil: [ classes at: name ifAbsentPut: [ (ClassOrganizer new allSubclassesOf: Object)
								detect: [ :cls | cls stonName == name ]
								ifNone: [ self error: 'Cannot resolve class named ' , name printString ] ] ] ]

]

{ #category : '*ston-gemstonebase' }
STONReader class >> new [
  ^ self basicNew
    initialize;
    yourself

]
