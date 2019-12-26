Class {
	#name : 'RwIndependentComponentVisitorV2',
	#superclass : 'RwAbstractProjectComponentVisitorV2',
	#instVars : [
		'packageNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'private' }
RwAbstractProjectComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self packageNames addAll: somePackageNames

]

{ #category : 'initialization' }
RwAbstractProjectComponentVisitorV2 >> initialize [
	super initialize.
	packageNames := Set new.
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> packageNames [

	^ packageNames
]
