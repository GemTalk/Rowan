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
		'maxIndexedVars',
		'compileErrorArray',
		'isOop',
		'instVarNames',
		'instVarsAreRemovable',
		'isDictionary',
		'isVariable',
		'selectionOop'
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
        (self stripOutUnicode: (dynamic at: i))
          -> (Reflection oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) ]
]

{ #category : 'command support' }
RowanInspectorService >> addFirstIndexedVars: anObject [
  | max |
  self setIndexedSize: anObject.
  max := indexedSize min: maxIndexedVars.
  objects addAll: (self indexedVarsFrom: 1 to: max).
  visibleIndices := max
]

{ #category : 'command support' }
RowanInspectorService >> addInstVars: anObject [
  | namedSize isRcBag |
  instVarNames := anObject class allInstVarNames.
  namedSize := instVarNames size.
  isRcBag := self isRcBag: anObject.
  1 to: namedSize do: [ :i | 
    objects
      add:
        ('-' , (self stripOutUnicode: (instVarNames at: i) asString))
          -> (Reflection oopOf: (anObject instVarAt: i)) ]
]

{ #category : 'client commands' }
RowanInspectorService >> addObject: string after: index [
  | newObject theObject |
  theObject := Object _objectForOop: oop.
  newObject := string evaluate.
  theObject class isIndexable
    ifTrue: [ theObject insertAll: (Array with: newObject) at: index ]
    ifFalse: [ theObject add: newObject ].
  selectionOop := newObject asOop.
  objects := OrderedCollection new.
  isOop := true.
  self inspect: oop.
  theObject class isIndexable
    ifTrue: [ self inspect: oop from: visibleIndices to: visibleIndices + 1 ]
]

{ #category : 'client commands' }
RowanInspectorService >> executeThenInspect: string context: anOop inWindow: handle [
  | answer |
  answer := RowanAnsweringService new basicExec: string context: anOop.
  answer key
    ifTrue: [ 
      RowanBrowserService new saveRootObject: answer value windowHandle: handle.
      self inspect: answer value ]
    ifFalse: [ 
      compileErrorArray := answer value.
      RowanCommandResult addResult: self ]
]

{ #category : 'client commands' }
RowanInspectorService >> executeThenInspect: string inFrame: level process: processOop context: contextOop inWindow: handle [
  | answer |
  answer := RowanAnsweringService new exec: string inFrame: level ofProcess: processOop context: contextOop.
  answer key
    ifTrue: [ 
      RowanBrowserService new saveRootObject: answer value windowHandle: handle.
      self inspect: answer value ]
    ifFalse: [ 
      compileErrorArray := answer value.
      RowanCommandResult addResult: self ]
]

{ #category : 'command support' }
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
      indexStart to: indexStop do: [ :i | 
        indexedVars
          add:
            (self stripOutUnicode: i printString) -> (Reflection oopOf: (aBag _at: i)) ] ]
    ifFalse: [ 
      indexStart to: indexStop do: [ :i | 
        indexedVars
          add:
            (self stripOutUnicode: i printString)
              -> (Reflection oopOf: (anObject at: i)) ] ].
  ^ indexedVars
]

{ #category : 'initialization' }
RowanInspectorService >> initialize [
  super initialize.
  objects := OrderedCollection new.
  instVarsAreRemovable := false.
]

{ #category : 'client commands' }
RowanInspectorService >> inspect: oopOrObject [
  ^ self inspect: oopOrObject inWindow: nil
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

{ #category : 'client commands' }
RowanInspectorService >> inspect: oopOrObject inWindow: handle [
  | anObject |
  self setOopFrom: oopOrObject.
  handle
    ifNotNil: [ RowanBrowserService new saveRootObject: oop windowHandle: handle ].
  anObject := Object _objectForOop: oop.
  isVariable := anObject class isVariable.
  (self isClientForwarder: anObject)
    ifTrue: [ ^ self inspectClientForwarder: anObject ].
  className := anObject class name.
  myself := 'self' -> (self selfPrintString: anObject).
  (anObject isKindOf: Dictionary superclass)
    ifTrue: [ ^ objects addAll: (self inspectDictionary: anObject from: 1 to: maxIndexedVars) ].
  self addInstVars: anObject.
  self addDynamicInstVars: anObject.
  self addFirstIndexedVars: anObject.
  RowanCommandResult addResult: self
]

{ #category : 'command support' }
RowanInspectorService >> inspectClientForwarder: anObject [

	oop := Reflection oopOf: anObject. 
	myself := 'self' -> anObject clientObject printString. 
	RowanCommandResult addResult: self.
]

{ #category : 'command support' }
RowanInspectorService >> inspectDictionary: aDictionary from: start to: stop [
  | sortedKeys |
  isDictionary := true.
  instVarsAreRemovable := true.
  self addInstVars: aDictionary.
  self addDynamicInstVars: aDictionary.
  sortedKeys := aDictionary keys asSortedCollection
    sortBlock: [ :x :y | x printString < y printString ].
  visibleIndices := sortedKeys size min: stop.
  nextIndices := OrderedCollection new.
  (sortedKeys copyFrom: start to: visibleIndices)
    do: [ :key | 
      nextIndices
        add:
          (self stripOutUnicode: key printString)
            ->
              (Array
                with: (Reflection oopOf: key)
                with: (Reflection oopOf: (aDictionary at: key))) ].
  indexedSize := aDictionary size.
  RowanCommandResult addResult: self.
  ^ nextIndices
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
  | stop anObject |
  anObject := Object _objectForOop: oop.
  (anObject isKindOf: Dictionary superclass)
    ifTrue: [ 
      ^ objects
        addAll:
          (self inspectDictionary: anObject from: indexStart to: indexStop) ].
  stop := indexStop min: indexedSize.
  nextIndices := self indexedVarsFrom: indexStart to: stop.
  visibleIndices := stop.
  RowanCommandResult addResult: self
]

{ #category : 'accessing' }
RowanInspectorService >> oop: anInteger [

	oop := anInteger
]

{ #category : 'client commands' }
RowanInspectorService >> removeIndexedInstVarAt: index [
  | theObject removalObject formerlyVisible |
  theObject := Object _objectForOop: oop.
  removalObject := theObject at: index.
  theObject remove: removalObject.
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.	"reinspect"
  self inspect: oop from: visibleIndices to: formerlyVisible	"pick up any additional indexed inst vars the user may have asked for"
]

{ #category : 'client commands' }
RowanInspectorService >> removeKey: keyOop [
  | theObject formerlyVisible removalKey |
  theObject := Object _objectForOop: oop.
  removalKey := Object _objectForOop: keyOop.
  theObject removeKey: removalKey.
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.	"reinspect"
  "self inspect: oop from: visibleIndices to: formerlyVisible"	"pick up any additional indexed keys the user may have asked for"
]

{ #category : 'printing' }
RowanInspectorService >> selfPrintString: anObject [
  ^ [ self stripOutUnicode: anObject printString ]
    on: Error
    do: [ :ex | 
      | printString |
      printString := 'Error printing object with oop ' , oop printString
        , '. Error text: ' , ex printString ]
]

{ #category : 'private' }
RowanInspectorService >> setIndexedSize: anObject [
  indexedSize := anObject class isVariable
    ifFalse: [ 0 ]
    ifTrue: [ 
      instVarsAreRemovable := true.
      (self shouldReferenceBySize: anObject)
        ifTrue: [ anObject size ]
        ifFalse: [ anObject _primitiveSize - anObject class allInstVarNames size ] ]
]

{ #category : 'private' }
RowanInspectorService >> setOopFrom: oopOrObject [
  oop := isOop
    ifTrue: [ oopOrObject ]
    ifFalse: [ Reflection oopOf: oopOrObject ]
]

{ #category : 'testing' }
RowanInspectorService >> shouldReferenceBySize: anObject [

	(self isRcBag: anObject) ifTrue:[^true].
	(anObject isKindOf: MultiByteString) ifTrue:[^true]. 
	^false
]
