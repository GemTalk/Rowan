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

{ #category : 'command support' }
RowanInspectorService >> addDynamicInstVars: anObject [
  | dynamic dynamicSize |
  dynamic := anObject dynamicInstanceVariables.
  dynamicSize := dynamic size.
  1 to: dynamicSize do: [ :i | 
    objects
      add:
        (dynamic at: i)
          -> (Reflection oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) ]
]

{ #category : 'command support' }
RowanInspectorService >> addIndexedVars: anObject [
  | indexedSize namedSize isRcBag |
  namedSize := anObject class allInstVarNames size.
  isRcBag := self isRcBag: anObject.
  indexedSize := (anObject class isNsc or: [ anObject class isIndexable ])
    ifFalse: [ 0 ]
    ifTrue: [ 
      isRcBag
        ifTrue: [ anObject size ]
        ifFalse: [ anObject _primitiveSize - namedSize ] ].
  isRcBag
    ifTrue: [ 
      | aBag |
      aBag := anObject _asIdentityBag.
      1 to: indexedSize do: [ :i | objects add: i printString -> (Reflection oopOf: (aBag _at: i)) ] ]
    ifFalse: [ 
      1 to: indexedSize do: [ :i | 
        objects
          add:
            i printString -> (Reflection oopOf: (anObject _primitiveAt: i + namedSize)) ] ]
]

{ #category : 'command support' }
RowanInspectorService >> addInstVars: anObject [
  | instVarNames namedSize isRcBag |
  instVarNames := anObject class allInstVarNames.
  namedSize := instVarNames size.
  isRcBag := self isRcBag: anObject.
  1 to: namedSize do: [ :i | objects add: (instVarNames at: i) -> (Reflection oopOf: (anObject instVarAt: i)) ]
]

{ #category : 'initialization' }
RowanInspectorService >> initialize [

	super initialize. 
	objects := Array new.
]

{ #category : 'client commands' }
RowanInspectorService >> inspect: anOop [
  | anObject |
  self rowanFixMe.	"to do - max size, unicode, "
  anObject := Object _objectForOop: anOop.
  (self isClientForwarder: anObject)
    ifTrue: [ ^ self inspectClientForwarder: anObject ].
  className := anObject class name.
  myself := 'self' -> anObject printString.
  (anObject isKindOf: Dictionary)
    ifTrue: [ ^ self inspectDictionary: anObject ].
  self addInstVars: anObject.
  self addDynamicInstVars: anObject.
  self addIndexedVars: anObject.
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

{ #category : 'testing' }
RowanInspectorService >> isClientForwarder: anObject [

	^(Reflection classOf: anObject) name == #'ClientForwarder'
]

{ #category : 'testing' }
RowanInspectorService >> isRcBag: anObject [
  ^ anObject class name == #'RcIdentityBag'
]

{ #category : 'accessing' }
RowanInspectorService >> oop: anInteger [

	oop := anInteger
]
