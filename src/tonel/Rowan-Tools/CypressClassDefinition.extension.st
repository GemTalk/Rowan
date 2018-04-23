Extension { #name : 'CypressClassDefinition' }

{ #category : '*rowan-tools' }
CypressClassDefinition >> definitionString [

	| classType type |
	type := self subclassType.
	type = ''
		ifTrue: [ classType := 'normal' ]
		ifFalse: [ 
			(type = 'indexableSubclass' or: [ type = 'variable' ])
				ifTrue: [ classType := 'variable' ]
				ifFalse: [ 
					type = 'byteSubclass'
						ifTrue: [ classType := 'bytes' ]
						ifFalse: [ self error: 'unknown subclass type: ' , type ] ] ].
	^ Rowan projectTools browser
		classCreationTemplateForSubclassOf: superclassName
		className: self name printString
		type: classType
		instanceVariablesString: self instanceVariablesString
		classVariablesString: self classVariablesString
		classInstanceVariablesString: self classInstanceVariablesString
		poolDictionariesString: self poolDictionariesString
		comment: self comment printString
		category: self category printString

]
