Class {
	#name : 'RwProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectConfiguration',
	#instVars : [
		'conditionalPackages',
		'conditionalPackageMapSpecs'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'visiting' }
RwProjectLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitProjectLoadConfiguration: self

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> conditionalPackageMapSpecs [

	^conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> conditionalPackages [

	^conditionalPackages ifNil: [ conditionalPackages := Dictionary new ]

]
