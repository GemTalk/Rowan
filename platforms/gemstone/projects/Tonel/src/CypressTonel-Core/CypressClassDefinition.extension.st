Extension { #name : 'CypressClassDefinition' }

{ #category : '*cypresstonel-core' }
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

{ #category : '*cypresstonel-core' }
CypressClassDefinition class >> name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment type: type [
  | st |
  st := type.
  st == #'normal'
    ifTrue: [ st := '' ].
  ^ self
    name: aClassName
    superclassName: aSuperclassName
    category: aCategory
    instVarNames: someInstanceVariableNames
    classInstVarNames: someClassInstanceVariableNames
    classVarNames: someClassVariableNames
    poolDictionaryNames: somePoolDictionaryNames
    comment: aComment
    subclassType: st

]

{ #category : '*cypresstonel-core' }
CypressClassDefinition class >> name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames 
	classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames 
	poolDictionaryNames: somePoolDictionaryNames gs_options: gs_options gs_constraints: gs_constraints comment: aComment 
	subclassType: subclassType [

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
		gs_options: gs_options 
		gs_constraints: gs_constraints
		comment: (self normalizeLineEndingsOf: aComment)
		subclassType: subclassType asString

]

{ #category : '*cypresstonel-core' }
CypressClassDefinition class >> name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames 
	classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames 
	poolDictionaryNames: somePoolDictionaryNames  gs_options: someGs_options gs_constraints: someGs_constraints
	comment: aComment type: type [
  | st |
  st := type.
  st == #'normal'
    ifTrue: [ st := '' ].
  ^ self
    name: aClassName
    superclassName: aSuperclassName
    category: aCategory
    instVarNames: someInstanceVariableNames
    classInstVarNames: someClassInstanceVariableNames
    classVarNames: someClassVariableNames
    poolDictionaryNames: somePoolDictionaryNames
    gs_options: someGs_options 
    gs_constraints: someGs_constraints
    comment: aComment
    subclassType: st

]

{ #category : '*cypresstonel-core' }
CypressClassDefinition >> hasClassTraitComposition [

	^false

]

{ #category : '*cypresstonel-core' }
CypressClassDefinition >> hasComment [
  ^ comment notNil and: [ comment ~= '' ]

]

{ #category : '*cypresstonel-core' }
CypressClassDefinition >> hasTraitComposition [

	^false

]

{ #category : '*cypresstonel-core' }
CypressClassDefinition >> isClassDefinition [
  ^ true

]

{ #category : '*cypresstonel-core' }
CypressClassDefinition >> poolDictionaries [

	^self poolDictionaryNames

]

{ #category : '*cypresstonel-core' }
CypressClassDefinition >> sortKey [
	^ self className

]

{ #category : '*cypresstonel-core' }
CypressClassDefinition >> type [
  | st |
  st := self subclassType.
  st = ''
    ifTrue: [ ^ #'normal' ].
  ^ st

]
