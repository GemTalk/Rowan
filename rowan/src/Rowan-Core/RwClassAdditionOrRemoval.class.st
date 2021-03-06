"
No class-specific documentation for CypClassAdditionOrRemoval, hierarchy is: 
Object
  CypClassAdditionOrRemoval( packageDefinition classKey classesModification)

"
Class {
	#name : 'RwClassAdditionOrRemoval',
	#superclass : 'Object',
	#instVars : [
		'projectDefinition',
		'packageDefinition',
		'classKey',
		'classesModification'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwClassAdditionOrRemoval class >> packageDefinition: aPackageDefinition classKey: aClassKey classesModification: aClassesModification [

	^(self new)
		packageDefinition: aPackageDefinition;
		classKey: aClassKey;
		classesModification: aClassesModification;
		yourself
]

{ #category : 'instance creation' }
RwClassAdditionOrRemoval class >> projectDefinition: aProjectDefinition packageDefinition: aPackageDefinition classKey: aClassKey classesModification: aClassesModification [

	^(self new)
		projectDefinition: aProjectDefinition;
		packageDefinition: aPackageDefinition;
		classKey: aClassKey;
		classesModification: aClassesModification;
		yourself

]

{ #category : 'Accessing' }
RwClassAdditionOrRemoval >> classesModification [
	^classesModification

]

{ #category : 'Updating' }
RwClassAdditionOrRemoval >> classesModification: newValue [
	classesModification := newValue

]

{ #category : 'Accessing' }
RwClassAdditionOrRemoval >> classKey [
	^classKey

]

{ #category : 'Updating' }
RwClassAdditionOrRemoval >> classKey: newValue [
	classKey := newValue

]

{ #category : 'Accessing' }
RwClassAdditionOrRemoval >> packageDefinition [
	^packageDefinition

]

{ #category : 'Updating' }
RwClassAdditionOrRemoval >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'Accessing' }
RwClassAdditionOrRemoval >> projectDefinition [
	^projectDefinition

]

{ #category : 'Updating' }
RwClassAdditionOrRemoval >> projectDefinition: newValue [
	projectDefinition := newValue

]
