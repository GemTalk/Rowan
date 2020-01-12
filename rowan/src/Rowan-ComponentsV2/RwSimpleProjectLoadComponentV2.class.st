Class {
	#name : 'RwSimpleProjectLoadComponentV2',
	#superclass : 'RwBasicProjectLoadComponentV2',
	#instVars : [
		'packageNames',
		'componentNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'validation' }
RwSimpleProjectLoadComponentV2 >> _validatedPackageNames [
	"answer the validated set of package names"

	^ self packageNames asSet
]

{ #category : 'visiting' }
RwSimpleProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	^ aVisitor visitSimpleProjectLoadComponent: self
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> addPackageNames: aPackageNames [
	"add packages to default conditional/group if applicable"

	self packageNames addAll: aPackageNames.
	packageNames := packageNames asSet asArray
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> componentNames [

	^ componentNames
]

{ #category : 'initialization' }
RwSimpleProjectLoadComponentV2 >> initialize [
	super initialize.
	packageNames := {}.
	componentNames := {}
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> packageNames [

	^ packageNames
]
