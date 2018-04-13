Extension { #name : 'CypressClassDefinition' }

{ #category : '*rowan-cypress' }
CypressClassDefinition >> createOrReviseClass [

	^ self subclassType = ''
		ifTrue: [ self createOrReviseRegularClass ]
		ifFalse: [ 
			self subclassType = 'byteSubclass'
				ifTrue: [ self createOrReviseByteClass ]
				ifFalse: [ 
					| typ |
					typ := self subclassType.
					(typ = 'indexableSubclass' or: [ typ = #'variable' ])
						ifTrue: [ self createOrReviseIndexableClass ]
						ifFalse: [ self error: 'unknown subclass type: ' , self subclassType printString ] ] ]
]
