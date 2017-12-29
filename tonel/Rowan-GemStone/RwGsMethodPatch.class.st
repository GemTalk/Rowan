"
No class-specific documentation for CypGsMethodPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodPatch',
	#superclass : 'RwGsPatch',
	#instVars : [
		'isMeta',
		'methodDefinition',
		'classDefinition',
		'packageDefinition',
		'behavior',
		'selector',
		'compiledMethod'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsMethodPatch class >> forMethod: aMethodDefinition isMeta: isMeta inClass: aClassDefinition inPackage: aPackageDefinition [

	^(self new)
		methodDefinition: aMethodDefinition;
		isMeta: isMeta;
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'accessing' }
RwGsMethodPatch >> classDefinition: newValue [
	classDefinition := newValue

]

{ #category : 'accessing' }
RwGsMethodPatch >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'applying' }
RwGsMethodPatch >> markPackageNotDirty [

	self markNotDirtyPackageNamed: packageDefinition key
]

{ #category : 'accessing' }
RwGsMethodPatch >> methodDefinition [
	^methodDefinition

]

{ #category : 'accessing' }
RwGsMethodPatch >> methodDefinition: newValue [
	methodDefinition := newValue

]

{ #category : 'accessing' }
RwGsMethodPatch >> packageDefinition [
	^packageDefinition

]

{ #category : 'accessing' }
RwGsMethodPatch >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'printing' }
RwGsMethodPatch >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: classDefinition key.
	isMeta ifTrue: [aStream nextPutAll: ' class'].
	aStream
		nextPutAll: '>>';
		nextPutAll: methodDefinition key
]

{ #category : 'initializers' }
RwGsMethodPatch >> runInitializer [
	"noop by default"
]
