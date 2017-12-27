"
No class-specific documentation for CypClassAdditionOrRemoval, hierarchy is: 
Object
  CypClassAdditionOrRemoval( packageDefinition classKey classesModification)

"
Class {
	#name : 'CypClassAdditionOrRemoval',
	#superclass : 'Object',
	#instVars : [
		'packageDefinition',
		'classKey',
		'classesModification'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypClassAdditionOrRemoval class >> packageDefinition: aPackageDefinition classKey: aClassKey classesModification: aClassesModification [

	^(self new)
		packageDefinition: aPackageDefinition;
		classKey: aClassKey;
		classesModification: aClassesModification;
		yourself
]

{ #category : 'Accessing' }
CypClassAdditionOrRemoval >> classesModification [
	^classesModification

]

{ #category : 'Updating' }
CypClassAdditionOrRemoval >> classesModification: newValue [
	classesModification := newValue

]

{ #category : 'Accessing' }
CypClassAdditionOrRemoval >> classKey [
	^classKey

]

{ #category : 'Updating' }
CypClassAdditionOrRemoval >> classKey: newValue [
	classKey := newValue

]

{ #category : 'Accessing' }
CypClassAdditionOrRemoval >> packageDefinition [
	^packageDefinition

]

{ #category : 'Updating' }
CypClassAdditionOrRemoval >> packageDefinition: newValue [
	packageDefinition := newValue

]
