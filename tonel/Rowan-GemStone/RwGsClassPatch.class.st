"
No class-specific documentation for CypGsClassPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassPatch( classDefinition packageDefinition)

"
Class {
	#name : 'RwGsClassPatch',
	#superclass : 'RwGsPatch',
	#instVars : [
		'classDefinition',
		'packageDefinition'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsClassPatch class >> for: aClassDefinition inPackage: aPackageDefinition [

	^(self new)
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'accessing' }
RwGsClassPatch >> classDefinition [
	^classDefinition

]

{ #category : 'accessing' }
RwGsClassPatch >> classDefinition: newValue [
	classDefinition := newValue

]

{ #category : 'accessing' }
RwGsClassPatch >> className [

	^classDefinition key
]

{ #category : 'applying' }
RwGsClassPatch >> markPackageNotDirty [

	self markNotDirtyPackageNamed: packageDefinition key
]

{ #category : 'accessing' }
RwGsClassPatch >> packageDefinition [
	^packageDefinition

]

{ #category : 'accessing' }
RwGsClassPatch >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'accessing' }
RwGsClassPatch >> packageName [

	^packageDefinition key
]

{ #category : 'accessing' }
RwGsClassPatch >> superclassName [

	^classDefinition superclassName
]
