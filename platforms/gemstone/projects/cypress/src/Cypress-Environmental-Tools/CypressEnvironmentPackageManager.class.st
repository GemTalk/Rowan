Class {
	#name : 'CypressEnvironmentPackageManager',
	#superclass : 'CypressPackageManager3',
	#instVars : [
		'defaultSymbolList',
		'lookupSymbolList',
		'compilationSymbolList',
		'defaultEnvironmentId'
	],
	#category : 'Cypress-Environmental-Tools'
}

{ #category : 'accessing' }
CypressEnvironmentPackageManager >> compilationSymbolList [
  ^ compilationSymbolList
    ifNil: [ compilationSymbolList := self defaultSymbolList ]

]

{ #category : 'accessing' }
CypressEnvironmentPackageManager >> compilationSymbolList: anObject [

   compilationSymbolList := anObject

]

{ #category : 'accessing' }
CypressEnvironmentPackageManager >> defaultEnvironmentId [

   ^defaultEnvironmentId

]

{ #category : 'accessing' }
CypressEnvironmentPackageManager >> defaultEnvironmentId: anObject [

   defaultEnvironmentId := anObject

]

{ #category : 'accessing' }
CypressEnvironmentPackageManager >> defaultSymbolList [

   ^defaultSymbolList

]

{ #category : 'accessing' }
CypressEnvironmentPackageManager >> defaultSymbolList: anObject [

   defaultSymbolList := anObject

]

{ #category : 'loading' }
CypressEnvironmentPackageManager >> loadResolvedReferences [
  | cypressLoader |
  cypressLoader := CypressEnvironmentLoader new.
  cypressLoader
    defaultSymbolDictionaryName: self defaultSymbolDictionaryName;
    compilationSymbolList: self compilationSymbolList;
    lookupSymbolList: self lookupSymbolList;
    defaultEnvironmentId: self defaultEnvironmentId.
  self resolvedPackageReferences
    do: [ :resolvedReference | 
      | package repository snapshot |
      package := resolvedReference packageDefinition.
      repository := resolvedReference repository.
      snapshot := (repository
        readPackageStructureForPackageNamed: resolvedReference name) snapshot.
      cypressLoader updatePackage: package withSnapshot: snapshot ].
  cypressLoader load.
  cypressLoader unloadable notEmpty
    ifTrue: [ self error: 'Unloadable definitions' ].
  cypressLoader errors notEmpty
    ifTrue: [ self error: 'Load errors' ].
  cypressLoader requirements notEmpty
    ifTrue: [ self error: 'Missing Requirements' ]

]

{ #category : 'loading' }
CypressEnvironmentPackageManager >> loadResolvedReferences: fileNames [
  | cypressLoader doUnloads |
  cypressLoader := CypressEnvironmentLoader new.
  cypressLoader
    defaultSymbolDictionaryName: self defaultSymbolDictionaryName;
    compilationSymbolList: self compilationSymbolList;
    lookupSymbolList: self lookupSymbolList;
    defaultEnvironmentId: self defaultEnvironmentId.
  self resolvedPackageReferences
    do: [ :resolvedReference | 
      | package repository snapshot |
      package := resolvedReference packageDefinition.
      repository := resolvedReference repository.
      snapshot := (repository
        readPackageStructureForPackageNamed: resolvedReference name
        files: fileNames ) snapshot.
      cypressLoader updatePackage: package withSnapshot: snapshot ].
  doUnloads := false .
  cypressLoader load: doUnloads .
  doUnloads ifTrue:[ cypressLoader unloadable notEmpty
      ifTrue: [ self error: 'Unloadable definitions' ]].
  cypressLoader errors notEmpty
    ifTrue: [ self error: 'Load errors' ].
  doUnloads ifTrue:[ cypressLoader requirements notEmpty
      ifTrue: [ self error: 'Missing Requirements' ]].

]

{ #category : 'accessing' }
CypressEnvironmentPackageManager >> lookupSymbolList [
  ^ lookupSymbolList ifNil: [ lookupSymbolList := self defaultSymbolList ]

]

{ #category : 'accessing' }
CypressEnvironmentPackageManager >> lookupSymbolList: anObject [

   lookupSymbolList := anObject

]
