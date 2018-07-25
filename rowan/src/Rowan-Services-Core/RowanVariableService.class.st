Class {
	#name : 'RowanVariableService',
	#superclass : 'RowanService',
	#instVars : [
		'oop',
		'key',
		'value'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'other' }
RowanVariableService class >> oop: oop key: key value: value [

	^self basicNew
		 oop: oop key: key value: value;
		yourself

]

{ #category : 'other' }
RowanVariableService >> oop: anInteger key: nameString value: valueString [

	oop := anInteger.
	key := nameString.
	value := valueString.

]
