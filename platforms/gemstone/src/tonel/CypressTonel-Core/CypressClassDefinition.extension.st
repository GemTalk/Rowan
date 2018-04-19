Extension { #name : 'CypressClassDefinition' }

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
