Class {
	#name : 'RowanSpecificationService',
	#superclass : 'RowanInspectorService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'command support' }
RowanSpecificationService >> addInstVars: anObject [
	"coerce the oops into strings for display in the browsers"

	super addInstVars: anObject.
	1 to: objects size do: [ :index | 
		objects
			at: index
			put:
				(objects at: index) key
					-> (Object _objectForOop: (objects at: index) value) printString ]
]

{ #category : 'initialization' }
RowanSpecificationService >> initialize: anOop [
	"intention revealing - not really an inspect"
	isOop := true. 
	maxIndexedVars := 0. 
	super inspect: anOop
]

{ #category : 'constants' }
RowanSpecificationService >> instVarPrefix [
	^ String new
]
