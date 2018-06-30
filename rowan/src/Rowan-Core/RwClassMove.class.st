"
Represents the move of a class definition from one package to another.
"
Class {
	#name : 'RwClassMove',
	#superclass : 'RwMove',
	#instVars : [
		'classBefore',
		'classAfter'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwClassMove class >> classBefore: beforeClassDefinition classAfter: afterClassDefinition packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition [

	^(self new)
		classBefore: beforeClassDefinition;
		classAfter: afterClassDefinition;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		yourself
]

{ #category : 'Accessing' }
RwClassMove >> classAfter [
	^classAfter

]

{ #category : 'Updating' }
RwClassMove >> classAfter: newValue [
	classAfter := newValue

]

{ #category : 'Accessing' }
RwClassMove >> classBefore [
	^classBefore

]

{ #category : 'Updating' }
RwClassMove >> classBefore: newValue [
	classBefore := newValue

]
