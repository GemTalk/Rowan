Class {
	#name : 'RowanSpecificationService',
	#superclass : 'RowanInspectorService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'command support' }
RowanSpecificationService >> addInstVars: anObject [
	| namedSize |
	instVarNames := anObject class allInstVarNames.
	namedSize := instVarNames size.
	1 to: namedSize do: [ :i | 
		objects
			add:
				(self stripOutUnicode: (instVarNames at: i) asString) -> (anObject instVarAt: i) ]
]

{ #category : 'initialization' }
RowanSpecificationService >> initialize: anOop [
	"intention revealing"
	isOop := true. 
	super inspect: anOop
]
