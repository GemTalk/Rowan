Class {
	#name : 'RwAbstractProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectConfiguration',
	#instVars : [
		'conditionalPackages'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> conditionalPackages [

	^conditionalPackages ifNil: [ conditionalPackages := Dictionary new ]

]
