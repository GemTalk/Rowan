Class {
	#name : 'CypressEnvironmentLoader',
	#superclass : 'CypressLoader',
	#instVars : [
		'defaultEnvironmentId',
		'lookupSymbolList',
		'compilationSymbolList'
	],
	#category : 'Cypress-Environmental-Tools'
}

{ #category : 'accessing' }
CypressEnvironmentLoader >> allClasses [
  | classes |
  classes := Array new.
  self defaultSymbolList
    do: [ :dict | classes addAll: (dict select: [ :each | each isBehavior ]) ].
  ^ classes

]

{ #category : 'loading' }
CypressEnvironmentLoader >> analyzeAdditions [
  | sorter |
  sorter := CypressEnvironmentDependencySorter new
    addAll: self additions;
    addExternalProvisions: self provisions;
    yourself.
  additions := sorter orderedItems.
  requirements := sorter externalRequirements.
  unloadable := sorter required

]

{ #category : 'loading' }
CypressEnvironmentLoader >> analyzeRemovals [
  | sorter |
  sorter := CypressEnvironmentDependencySorter new
    addAll: self removals;
    yourself.
  removals := sorter orderedItems reverse

]

{ #category : 'accessing' }
CypressEnvironmentLoader >> compilationSymbolList [
  ^ compilationSymbolList
    ifNil: [ compilationSymbolList := self defaultSymbolList ]

]

{ #category : 'accessing' }
CypressEnvironmentLoader >> compilationSymbolList: anObject [

   compilationSymbolList := anObject

]

{ #category : 'accessing' }
CypressEnvironmentLoader >> defaultEnvironmentId [
  ^ defaultEnvironmentId ifNil: [ defaultEnvironmentId := 0 ]

]

{ #category : 'accessing' }
CypressEnvironmentLoader >> defaultEnvironmentId: anObject [

   defaultEnvironmentId := anObject

]

{ #category : 'accessing' }
CypressEnvironmentLoader >> defaultSymbolList [
  ^ System myUserProfile symbolList

]

{ #category : 'operations' }
CypressEnvironmentLoader >> loadClassDefinition: aPatchOperation [
  [ 
  aPatchOperation
    loadClassDefinition: self defaultSymbolDictionaryName
    environmentLoader: self ]
    on: Error
    do: [ :ex | self handlePatchOperation: aPatchOperation failure: ex ]

]

{ #category : 'operations' }
CypressEnvironmentLoader >> loadMethodDefinition: aPatchOperation [
  [ 
  [ 
  aPatchOperation
    loadMethodDefinition: self lookupSymbolList
    environmentLoader: self ]
    on: CompileError
    do: [ :ex | self handleCompileError: ex from: aPatchOperation ] ]
    on: Error
    do: [ :ex | self handlePatchOperation: aPatchOperation failure: ex ]

]

{ #category : 'accessing' }
CypressEnvironmentLoader >> lookupSymbolList [
  ^ lookupSymbolList ifNil: [ lookupSymbolList := self defaultSymbolList ]

]

{ #category : 'accessing' }
CypressEnvironmentLoader >> lookupSymbolList: anObject [

   lookupSymbolList := anObject

]

{ #category : 'operations' }
CypressEnvironmentLoader >> postLoad: aPatchOperation [
  [ 
  aPatchOperation
    postLoadDefinition: lookupSymbolList
    environmentId: self defaultEnvironmentId ]
    on: Error
    do: [ :ex | self handlePatchOperation: aPatchOperation failure: ex ]

]
