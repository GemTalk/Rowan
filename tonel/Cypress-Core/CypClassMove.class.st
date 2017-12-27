"
Represents the move of a class definition from one package to another.
"
Class {
	#name : 'CypClassMove',
	#superclass : 'Object',
	#instVars : [
		'classBefore',
		'classAfter',
		'packageBefore',
		'packageAfter'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypClassMove class >> classBefore: beforeClassDefinition classAfter: afterClassDefinition packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition [

	^(self new)
		classBefore: beforeClassDefinition;
		classAfter: afterClassDefinition;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		yourself
]

{ #category : 'Accessing' }
CypClassMove >> classAfter [
	^classAfter

]

{ #category : 'Updating' }
CypClassMove >> classAfter: newValue [
	classAfter := newValue

]

{ #category : 'Accessing' }
CypClassMove >> classBefore [
	^classBefore

]

{ #category : 'Updating' }
CypClassMove >> classBefore: newValue [
	classBefore := newValue

]

{ #category : 'Accessing' }
CypClassMove >> packageAfter [
	^packageAfter

]

{ #category : 'Updating' }
CypClassMove >> packageAfter: newValue [
	packageAfter := newValue

]

{ #category : 'Accessing' }
CypClassMove >> packageBefore [
	^packageBefore

]

{ #category : 'Updating' }
CypClassMove >> packageBefore: newValue [
	packageBefore := newValue

]
