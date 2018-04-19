Extension { #name : 'CypressClassDefinition' }

 #category : '*rowan-cypress' }
CypressClassDefinition >> accept: aVisitor [
	aVisitor visitClassDefinition: self.

]

{ #category : '*rowan-cypress' }
CypressClassDefinition >> commentStamp [

	^ ''
]

{ #category : '*rowan-cypress' }
CypressClassDefinition >> hasClassInstanceVariables [

	^ self classInstVarNames isEmpty not
]

{ #category : '*rowan-cypress' }
CypressClassDefinition >> printDefinitionOn: stream [

        stream
            nextPutAll: self superclassName;
            space;
]

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
					(typ = 'indexableSubclass' or: [ typ = 'variable' ])
						ifTrue: [ self createOrReviseIndexableClass ]
						ifFalse: [ self error: 'unknown subclass type: ' , self subclassType printString ] ] ]

]

{ #category : '*rowan-cypress' }
CypressClassDefinition class >> name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment subclassType: subclassType [

	^ self new
		name: aClassName asString
		superclassName: aSuperclassName asString
		category: aCategory asString
		instVarNames:
			(someInstanceVariableNames asArray collect: [ :each | each asString ])
		classInstVarNames:
			(someClassInstanceVariableNames asArray collect: [ :each | each asString ])
		classVarNames:
			(someClassVariableNames asArray collect: [ :each | each asString ])
		poolDictionaryNames:
			(somePoolDictionaryNames asArray collect: [ :each | each asString ])
		comment: (self normalizeLineEndingsOf: aComment)
		subclassType: subclassType asString
]
