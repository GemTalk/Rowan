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
		'selectionOop',
		'isUnordered',
		'statusText'
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
        ('--' , (self stripOutUnicode: (dynamic at: i)))
          -> (Reflection oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) ]
]

{ #category : 'command support' }
RowanInspectorService >> addFirstIndexedVars: anObject [
  | max |
  self setIndexedSize: anObject.
  max := indexedSize min: maxIndexedVars.
  objects addAll: (self safeVariablesFrom: 1 to: max).
  visibleIndices := max
]

{ #category : 'command support' }
RowanInspectorService >> addInstVars: anObject [
  | namedSize |
  instVarNames := anObject class allInstVarNames.
  namedSize := instVarNames size.
  1 to: namedSize do: [ :i | 
    objects
      add:
        ('-' , (self stripOutUnicode: (instVarNames at: i) asString))
          -> (Reflection oopOf: (anObject instVarAt: i)) ]
]

{ #category : 'client commands' }
RowanInspectorService >> addKey: keyString [
  | newObject theObject |
  theObject := Object _objectForOop: oop.
  newObject := keyString
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject at: newObject ifAbsentPut: nil.
  selectionOop := newObject asOop.
  objects := OrderedCollection new.
  isOop := true.
  self inspect: oop
]

{ #category : 'client commands' }
RowanInspectorService >> addObject: string after: index [
  | newObject theObject |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject class isIndexable
    ifTrue: [ 
      | insertionCollection |
      insertionCollection := (theObject isKindOf: CharacterCollection)
        ifTrue: [ newObject ]
        ifFalse: [ Array with: newObject ].
      theObject insertAll: insertionCollection at: index + 1 ]
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

{ #category : 'initialization' }
RowanInspectorService >> initialize [
  super initialize.
  objects := OrderedCollection new.
  instVarsAreRemovable := false.
  isUnordered := false
]

{ #category : 'client commands' }
RowanInspectorService >> inspect: oopOrObject [
  ^ self inspect: oopOrObject inWindow: nil
]

{ #category : 'client commands' }
RowanInspectorService >> inspect: anOop from: indexStart to: indexStop [
  | anObject stop |
  anObject := Object _objectForOop: anOop.
  stop := (indexStop min: indexedSize) min: self maxVariables.
  nextIndices := self safeVariablesFrom: indexStart to: stop.
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
  | sortedKeys service |
  isDictionary := true.
  instVarsAreRemovable := true.
  self addInstVars: aDictionary.
  self addDynamicInstVars: aDictionary.
  service := RowanAnsweringService new.
  sortedKeys := (aDictionary keys
    collect: [ :key | 
      (service basicPrintStringOfObject: key toMaxSize: self maxPrintStringSize)
        -> key ]) asSortedCollection.
  visibleIndices := (sortedKeys size min: stop) min: self maxVariables.
  nextIndices := OrderedCollection new.
  (sortedKeys copyFrom: start to: visibleIndices)
    do: [ :assoc | 
      nextIndices
        add:
          assoc key
            ->
              (Array
                with: (Reflection oopOf: assoc value)
                with: (Reflection oopOf: (aDictionary at: assoc value))) ].
  indexedSize := aDictionary size.
  RowanCommandResult addResult: self.
  ^ nextIndices
]

{ #category : 'testing' }
RowanInspectorService >> isClientForwarder: anObject [

	^(Reflection classOf: anObject) name == #'ClientForwarder'
]

{ #category : 'constants' }
RowanInspectorService >> maxPrintStringSize [
  "currently matches Jadeite client"

  ^ 100000
]

{ #category : 'constants' }
RowanInspectorService >> maxVariables [
	"See Jadeite issue #668. Currently, 
	Dolphin doesn't seem to be able to 
	handle more than about 65k elements
	in a list
	https://github.com/GemTalk/Jadeite/issues/668#issuecomment-566795924" 
	
	^65000
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
  stop := (indexStop min: indexedSize) min: self maxVariables.
  nextIndices := self safeVariablesFrom: indexStart to: stop.
  visibleIndices := stop.
  RowanCommandResult addResult: self
]

{ #category : 'accessing' }
RowanInspectorService >> oop: anInteger [

	oop := anInteger
]

{ #category : 'private' }
RowanInspectorService >> reinspect: theObject [
  | formerlyVisible |
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.
  (visibleIndices > 0 and: [ visibleIndices < formerlyVisible ])
    ifTrue: [ self inspect: oop from: visibleIndices to: formerlyVisible ]
]

{ #category : 'client commands' }
RowanInspectorService >> removeDynamicInstVars: dynamicInstVarNames [
  | theObject |
  theObject := Object _objectForOop: oop.
  dynamicInstVarNames
    do: [ :dynamicInstVar | theObject removeDynamicInstVar: dynamicInstVar ].
  self reinspect: theObject
]

{ #category : 'client commands' }
RowanInspectorService >> removeIndexedInstVarsAt: indices [
  | theObject removalObjects |
  theObject := Object _objectForOop: oop.
  removalObjects := indices collect: [ :index | theObject at: index ].
  removalObjects do: [ :removalObject | theObject remove: removalObject ].
  self reinspect: theObject
]

{ #category : 'client commands' }
RowanInspectorService >> removeKeys: keyOops [
  | theObject removalKey |
  theObject := Object _objectForOop: oop.
  keyOops
    do: [ :keyOop | 
      removalKey := Object _objectForOop: keyOop.
      theObject removeKey: removalKey.
      visibleIndices := visibleIndices - 1 max: 0 ].
  self reinspect: theObject
]

{ #category : 'client commands' }
RowanInspectorService >> removeOop: elementOop [
  | theObject removal |
  theObject := Object _objectForOop: oop.
  removal := Object _objectForOop: elementOop.
  theObject remove: removal.
  self reinspect: theObject
]

{ #category : 'client commands' }
RowanInspectorService >> replaceElement: oldOop with: string [
  | theObject newObject formerlyVisible oldObject |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  oldObject := Object _objectForOop: oldOop.
  theObject remove: oldObject.
  theObject add: newObject.
  selectionOop := newObject asOop.
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.
  visibleIndices > 0
    ifTrue: [ self inspect: oop from: visibleIndices to: formerlyVisible ]
]

{ #category : 'command support' }
RowanInspectorService >> safeVariablesFrom: indexStart to: indexStop [
  [ ^ self variablesFrom: indexStart to: indexStop ]
    on: Error
    do: [ :ex | 
      indexedSize := 0.
      instVarsAreRemovable := false.
      visibleIndices := 0 to: 0.
      nextIndices := 0 to: 0.
      isVariable := false.
      statusText := 'Error getting object contents: ' , ex description.
      ^ OrderedCollection new ]
]

{ #category : 'client commands' }
RowanInspectorService >> save: string dynamicInstVar: instVarName [
  | theObject newObject |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject dynamicInstVarAt: instVarName put: newObject.
  self reinspect: theObject
]

{ #category : 'client commands' }
RowanInspectorService >> save: string indexedVarAt: index [
  | theObject newObject formerlyVisible |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject at: index put: newObject.
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.
  visibleIndices > 0
    ifTrue: [ self inspect: oop from: visibleIndices to: formerlyVisible ]
]

{ #category : 'client commands' }
RowanInspectorService >> save: string namedInstVar: instVarName [
  | theObject newObject instVarIndex |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  instVarIndex := theObject class allInstVarNames indexOf: instVarName.
  theObject instVarAt: instVarIndex put: newObject.
  self reinspect: theObject
]

{ #category : 'client commands' }
RowanInspectorService >> saveKey: keyOop value: string [
  | theObject newObject key |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  key := Object _objectForOop: keyOop.
  theObject at: key put: newObject.
  self reinspect: theObject
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

	(anObject isKindOf: UnorderedCollection) ifTrue:[^true].
	(anObject isKindOf: MultiByteString) ifTrue:[^true]. 
	^false
]

{ #category : 'command support' }
RowanInspectorService >> variablesFrom: indexStart to: indexStop [
  | namedSize anObject indexedVars |
  indexedVars := OrderedCollection new.
  anObject := Object _objectForOop: oop.
  namedSize := anObject class allInstVarNames size.
  isUnordered := anObject isKindOf: UnorderedCollection.
  isUnordered
    ifTrue: [ 
      | sorted service |
      service := RowanAnsweringService new.
      sorted := (anObject
        collect: [ :var | 
          (service basicPrintStringOfObject: var toMaxSize: self maxPrintStringSize)
            -> (Reflection oopOf: var) ]) asSortedCollection: [:x :y | x key < y key]. 
      indexStart to: indexStop do: [ :i | indexedVars add: (sorted at: i) ] ]
    ifFalse: [ indexStart to: indexStop do: [ :i | indexedVars add: i printString -> (Reflection oopOf: (anObject at: i)) ] ].
  ^ indexedVars
]
