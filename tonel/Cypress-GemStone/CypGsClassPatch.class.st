"
No class-specific documentation for CypGsClassPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassPatch( classDefinition packageDefinition)

"
Class {
	#name : 'CypGsClassPatch',
	#superclass : 'CypGsPatch',
	#instVars : [
		'classDefinition',
		'packageDefinition'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'instance creation' }
CypGsClassPatch class >> for: aClassDefinition inPackage: aPackageDefinition [

	^(self new)
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'accessing' }
CypGsClassPatch >> classDefinition [
	^classDefinition

]

{ #category : 'accessing' }
CypGsClassPatch >> classDefinition: newValue [
	classDefinition := newValue

]

{ #category : 'accessing' }
CypGsClassPatch >> className [

	^classDefinition key
]

{ #category : 'applying' }
CypGsClassPatch >> markPackageNotDirty [

	self markNotDirtyPackageNamed: packageDefinition key
]

{ #category : 'accessing' }
CypGsClassPatch >> packageDefinition [
	^packageDefinition

]

{ #category : 'accessing' }
CypGsClassPatch >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'accessing' }
CypGsClassPatch >> superclassName [

	^classDefinition superclassName
]
