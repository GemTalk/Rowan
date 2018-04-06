Extension { #name : 'RwGsMethodPatch' }

{ #category : '*rowan-gemstone' }
RwGsMethodPatch >> primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols [

	| className class |
	classDefinition key
		ifNil: [ 
			"class is being deleted ... we're done"
			^ self ].
	className := classDefinition key asSymbol.
	class := createdClasses
		at: className
		ifAbsent: [ 
			tempSymbols
				at: className
				ifAbsent: [ 
					"cannot find class ... caller can decide whether or not that is a problem"
					^ self ] ].
	behavior := isMeta
		ifTrue: [ class class ]
		ifFalse: [ class ]
]
