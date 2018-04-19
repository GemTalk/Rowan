Class {
	#name : 'TonelCypressReaderTest',
	#superclass : 'TonelReaderTest',
	#category : 'CypressTonel-Tests'
}

{ #category : 'tests' }
TonelCypressReaderTest >> assertClassDefinition: a and: b [
	self assert: a className equals: b className.
	self assert: a superclassName equals: b superclassName.
	self assert: a category equals: b category.	
	self assert: a instVarNames equals: b instVarNames.
	self assert: a classInstVarNames equals: b classInstVarNames.
	self assert: a classVarNames equals: b classVarNames.
	self assert: a poolDictionaries equals: b poolDictionaries.
	self assert: a type equals: b type.
	self assert: a comment equals: b comment.

]

{ #category : 'tests' }
TonelCypressReaderTest >> assertDefinition: a and: b [
	a isClassDefinition ifTrue: [ ^ self assertClassDefinition: a and: b ].
	a isMethodDefinition ifTrue: [ ^ self assertMethodDefinition: a and: b ].

]

{ #category : 'tests' }
TonelCypressReaderTest >> assertMethodDefinition: a and: b [
	self assert: a className equals: b className.
	self assert: a selector equals: b selector.
	self assert: a category equals: b category.
	self assert: a source asByteArray equals: b source asByteArray.
	self assert: a classIsMeta equals: b classIsMeta

]

{ #category : 'private' }
TonelCypressReaderTest >> fileUtils [
  ^ CypressFileUtilities current

]

{ #category : 'mocks' }
TonelCypressReaderTest >> mockSnapshot [
  ^ self mockCypressSnapshot

]
