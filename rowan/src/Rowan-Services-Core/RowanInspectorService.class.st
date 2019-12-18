Class {
	#name : 'RowanInspectorService',
	#superclass : 'RowanService',
	#instVars : [
		'oop',
		'objects',
		'myself',
		'className',
		'indexedSize',
		'visibleIndices',
		'nextIndices',
		'maxIndexedVars'
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
RowanInspectorService >> addFirstIndexedVars: anObject [
  | max |
  self setIndexedSize: anObject.
  max := indexedSize min: maxIndexedVars.
  objects := self indexedVarsFrom: 1 to: max.
  visibleIndices := max
]

{ #category : 'command support' }
RowanInspectorService >> addInstVars: anObject [
  | instVarNames namedSize isRcBag |
  instVarNames := anObject class allInstVarNames.
  namedSize := instVarNames size.
  isRcBag := self isRcBag: anObject.
  1 to: namedSize do: [ :i | objects add: (instVarNames at: i) -> (Reflection oopOf: (anObject instVarAt: i)) ]
]

{ #category : 'client commands' }
RowanInspectorService >> executeThenInspect: string context: anOop inWindow: handle [
  | answer |
  answer := RowanAnsweringService new basicExec: string context: anOop.
  answer key
    ifTrue: [ 
      RowanBrowserService new saveRootObject: answer value windowHandle: handle.
      self inspect: answer value ]
]

{ #category : 'private' }
RowanInspectorService >> indexedVarsFrom: indexStart to: indexStop [
  | namedSize isRcBag anObject indexedVars |
  indexedVars := OrderedCollection new.
  anObject := Object _objectForOop: oop.
  namedSize := anObject class allInstVarNames size.
  isRcBag := self isRcBag: anObject.
  isRcBag
    ifTrue: [ 
      | aBag |
      aBag := anObject _asIdentityBag.
      indexStart to: indexStop do: [ :i | indexedVars add: i printString -> (Reflection oopOf: (aBag _at: i)) ] ]
    ifFalse: [ 
      indexStart to: indexStop do: [ :i | 
        indexedVars
          add:
            i printString -> (Reflection oopOf: (anObject _primitiveAt: i + namedSize)) ] ].
  ^ indexedVars
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
  oop := anOop.
  anObject := Object _objectForOop: anOop.
  (self isClientForwarder: anObject)
    ifTrue: [ ^ self inspectClientForwarder: anObject ].
  className := anObject class name.
  myself := 'self' -> anObject printString.
  (anObject isKindOf: Dictionary)
    ifTrue: [ ^ self inspectDictionary: anObject ].
  self addInstVars: anObject.
  self addDynamicInstVars: anObject.
  self addFirstIndexedVars: anObject.
  RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanInspectorService >> inspect: anOop from: indexStart to: indexStop [
  | anObject stop |
  anObject := Object _objectForOop: anOop.
  stop := indexStop min: indexedSize.
  nextIndices := self indexedVarsFrom: indexStart to: stop.
  visibleIndices := stop.
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

{ #category : 'client commands' }
RowanInspectorService >> nextIndexedVarsFrom: indexStart to: indexStop [
  | anObject stop |
  anObject := Object _objectForOop: oop.
  stop := indexStop min: indexedSize.
  nextIndices := self indexedVarsFrom: indexStart to: stop.
  visibleIndices := stop.
  RowanCommandResult addResult: self
]

{ #category : 'accessing' }
RowanInspectorService >> oop: anInteger [

	oop := anInteger
]

{ #category : 'private' }
RowanInspectorService >> setIndexedSize: anObject [
  indexedSize := (anObject class isNsc or: [ anObject class isIndexable ])
    ifFalse: [ 0 ]
    ifTrue: [ 
      (self isRcBag: anObject)
        ifTrue: [ anObject size ]
        ifFalse: [ anObject _primitiveSize - anObject class allInstVarNames size ] ]
]
