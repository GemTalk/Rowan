Class {
	#name : 'RowanVariableService',
	#superclass : 'RowanService',
	#instVars : [
		'oop',
		'key',
		'value',
		'className'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'other' }
RowanVariableService class >> oop: anInteger key: nameString value: valueString className: classNameString [

	^self basicNew
		oop: anInteger key: nameString value: valueString className: classNameString;
		yourself

]

{ #category : 'other' }
RowanVariableService >> oop: anInteger key: nameString value: valueString className: classNameString [

	oop := anInteger.
	key := nameString.
	value := valueString.
	className := classNameString.

]
