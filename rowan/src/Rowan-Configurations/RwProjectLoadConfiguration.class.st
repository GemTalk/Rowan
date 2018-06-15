Class {
	#name : 'RwProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectLoadConfiguration',
	#instVars : [
		'definedGroupNames'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'visiting' }
RwProjectLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitProjectLoadConfiguration: self

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> definedGroupNames [

	^ definedGroupNames ifNil: [ definedGroupNames :=  Dictionary new ]

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> definedGroupNames: aColl [

	definedGroupNames := aColl

]

{ #category : 'initialization' }
RwProjectLoadConfiguration >> initialize [

	super initialize.
	definedGroupNames := Dictionary new

]
