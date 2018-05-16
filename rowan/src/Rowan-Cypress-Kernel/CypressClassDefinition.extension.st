Extension { #name : 'CypressClassDefinition' }

{ #category : '*rowan-cypress-kernel' }
CypressClassDefinition >> accept: aVisitor [
	aVisitor visitClassDefinition: self.

]

{ #category : '*rowan-cypress-kernel' }
CypressClassDefinition >> commentStamp [

	^ ''
]

{ #category : '*rowan-cypress-kernel' }
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

{ #category : '*rowan-cypress-kernel' }
CypressClassDefinition >> hasClassInstanceVariables [

	^ self classInstVarNames isEmpty not
]

{ #category : '*rowan-cypress-kernel' }
CypressClassDefinition >> printDefinitionOn: stream [

        stream
            nextPutAll: self superclassName;
            space;
            nextPutAll: self classCreationSelector;
            nextPut: $# ;
            nextPutAll: self className;
            cr; tab.
        stream
            nextPutAll: 'instanceVariableNames: ';
            store: self instanceVariablesString;
            cr; tab;
            nextPutAll: 'classVariableNames: ';
            store: self classVariablesString;
            cr; tab;
            nextPutAll: 'poolDictionaries: ';
            store: self poolDictionariesString;
            cr; tab;
            nextPutAll: 'category: ';
            store: self category asString 

]

{ #category : '*rowan-cypress-kernel' }
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
