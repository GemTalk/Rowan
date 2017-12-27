"
No class-specific documentation for CypGsMethodPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'CypGsMethodPatch',
	#superclass : 'CypGsPatch',
	#instVars : [
		'isMeta',
		'methodDefinition',
		'classDefinition',
		'packageDefinition',
		'behavior',
		'selector',
		'compiledMethod'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'instance creation' }
CypGsMethodPatch class >> forMethod: aMethodDefinition isMeta: isMeta inClass: aClassDefinition inPackage: aPackageDefinition [

	^(self new)
		methodDefinition: aMethodDefinition;
		isMeta: isMeta;
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'accessing' }
CypGsMethodPatch >> classDefinition: newValue [
	classDefinition := newValue

]

{ #category : 'accessing' }
CypGsMethodPatch >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'applying' }
CypGsMethodPatch >> markPackageNotDirty [

	self markNotDirtyPackageNamed: packageDefinition key
]

{ #category : 'accessing' }
CypGsMethodPatch >> methodDefinition [
	^methodDefinition

]

{ #category : 'accessing' }
CypGsMethodPatch >> methodDefinition: newValue [
	methodDefinition := newValue

]

{ #category : 'accessing' }
CypGsMethodPatch >> packageDefinition [
	^packageDefinition

]

{ #category : 'accessing' }
CypGsMethodPatch >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'printing' }
CypGsMethodPatch >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: classDefinition key.
	isMeta ifTrue: [aStream nextPutAll: ' class'].
	aStream
		nextPutAll: '>>';
		nextPutAll: methodDefinition key
]
