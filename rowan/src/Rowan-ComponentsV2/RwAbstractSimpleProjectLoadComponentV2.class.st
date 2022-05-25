Class {
	#name : 'RwAbstractSimpleProjectLoadComponentV2',
	#superclass : 'RwBasicProjectLoadComponentV2',
	#instVars : [
		'packageNames',
		'condition'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> packageNames [
	"duplication of subclass implementation until we've moved to V2.1, since this method is 
		used during project load and superclass methods removed before subclass methods added.
		This duplication will go away, once this obsolete class is removed (planned for v2.1)."

	^ packageNames
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> packageNames: anArray [
 packageNames := anArray
]

{ #category : 'copying' }
RwAbstractSimpleProjectLoadComponentV2 >> postCopy [
	super postCopy.
	packageNames := packageNames copy.
]
