Class {
	#name : 'RowanInspectorService',
	#superclass : 'RowanService',
	#instVars : [
		'oop',
		'objects',
		'myself',
		'className'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'initialization' }
RowanInspectorService >> initialize [

	super initialize. 
	objects := Array new.
]

{ #category : 'client commands' }
RowanInspectorService >> inspect: anOop [
  | anObject dynamic dynamicSize indexedSize instVarNames namedSize isRcBag |
  self rowanFixMe.	"to do - max size, unicode, "
  anObject := Object _objectForOop: anOop.
  (self isClientForwarder: anObject)
    ifTrue: [ ^ self inspectClientForwarder: anObject ].
  className := anObject class name.
  myself := 'self' -> anObject printString.
  (anObject isKindOf: Dictionary)
    ifTrue: [ ^ self inspectDictionary: anObject ].
  instVarNames := anObject class allInstVarNames.
  namedSize := instVarNames size.
  dynamic := anObject dynamicInstanceVariables.
  dynamicSize := dynamic size.
  isRcBag := anObject class name == #'RcIdentityBag'.
  indexedSize := (anObject class isNsc or: [ anObject class isIndexable ])
    ifFalse: [ 0 ]
    ifTrue: [ 
      isRcBag
        ifTrue: [ anObject size ]
        ifFalse: [ anObject _primitiveSize - namedSize ] ].
  1 to: namedSize do: [ :i | objects add: (instVarNames at: i) -> (Reflection oopOf: (anObject instVarAt: i)) ].
  1 to: dynamicSize do: [ :i | 
    objects
      add:
        (dynamic at: i)
          -> (Reflection oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) ].
  isRcBag
    ifTrue: [ 
      | aBag |
      aBag := anObject _asIdentityBag.
      1 to: indexedSize do: [ :i | objects add: i printString -> (Reflection oopOf: (aBag _at: i)) ] ]
    ifFalse: [ 
      1 to: indexedSize do: [ :i | 
        objects
          add:
            i printString -> (Reflection oopOf: (anObject _primitiveAt: i + namedSize)) ] ].
  RowanCommandResult addResult: self
]

{ #category : 'command support' }
RowanInspectorService >> inspectClientForwarder: anObject [

	oop := Reflection oopOf: anObject. 
	myself := 'self' -> anObject clientObject printString. 
	RowanCommandResult addResult: self.
]

{ #category : 'command support' }
RowanInspectorService >> inspectDictionary: aDictionary [
  aDictionary
    keysDo: [ :key | objects add: key printString -> (Reflection oopOf: (aDictionary at: key)) ].
  RowanCommandResult addResult: self
]

{ #category : 'command support' }
RowanInspectorService >> isClientForwarder: anObject [

	^(Reflection classOf: anObject) name == #'ClientForwarder'
]

{ #category : 'accessing' }
RowanInspectorService >> oop: anInteger [

	oop := anInteger
]
