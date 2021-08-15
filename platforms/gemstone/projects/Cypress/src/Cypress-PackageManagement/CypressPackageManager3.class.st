"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPackageManager3',
	#superclass : 'Object',
	#instVars : [
		'knownRepositories',
		'defaultSymbolDictionaryName',
		'resolvedPackageReferences'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'testing' }
CypressPackageManager3 class >> isPackageLoaded: aPackageName [

  ^ (CypressPackageDefinition named: aPackageName) snapshot definitions isEmpty not

]

{ #category : 'instance creation' }
CypressPackageManager3 class >> new [
  ^self basicNew initialize

]

{ #category : 'Updating' }
CypressPackageManager3 >> addRepository: aRepository [
  self knownRepositories at: aRepository url asString put: aRepository

]

{ #category : 'Updating' }
CypressPackageManager3 >> addResolvedReference: resolvedReference [
  self resolvedPackageReferences add: resolvedReference

]

{ #category : 'Querying' }
CypressPackageManager3 >> allResolvedPackageReferences [
  | resolved |
  resolved := OrderedCollection new.
  self knownRepositories
    keysAndValuesDo: [ :repoUrl :repo | 
      repo packageNames
        do: [ :packageName | resolved add: (CypressResolvedReference name: packageName repository: repo) ] ].
  ^ resolved asSortedCollection asArray

]

{ #category : 'initialization' }
CypressPackageManager3 >> defaultSymbolDictionaryName [

  ^defaultSymbolDictionaryName

]

{ #category : 'initialization' }
CypressPackageManager3 >> defaultSymbolDictionaryName: aStringOrNil [

  defaultSymbolDictionaryName := aStringOrNil

]

{ #category : 'Updating' }
CypressPackageManager3 >> initialize [
  self
    knownRepositories: Dictionary new;
    resolvedPackageReferences: OrderedCollection new;
    yourself

]

{ #category : 'Accessing' }
CypressPackageManager3 >> knownRepositories [

   ^knownRepositories

]

{ #category : 'Accessing' }
CypressPackageManager3 >> knownRepositories: anObject [

   knownRepositories := anObject

]

{ #category : 'loading' }
CypressPackageManager3 >> loadResolvedReference: resolvedReference [
  | cypressLoader package repository snapshot |
  cypressLoader := CypressLoader new.
  cypressLoader defaultSymbolDictionaryName: self defaultSymbolDictionaryName.
  package := resolvedReference packageDefinition.
  repository := resolvedReference repository.
  snapshot := (repository
    readPackageStructureForPackageNamed: resolvedReference name) snapshot.
  cypressLoader updatePackage: package withSnapshot: snapshot.
  cypressLoader load.
  cypressLoader unloadable notEmpty
    ifTrue: [ self error: 'Unloadable definitions' ].
  cypressLoader errors notEmpty
    ifTrue: [ self error: 'Load errors' ].
  cypressLoader requirements notEmpty
    ifTrue: [ self error: 'Missing Requirements' ]

]

{ #category : 'loading' }
CypressPackageManager3 >> loadResolvedReferences [
  | cypressLoader |
  cypressLoader := CypressLoader new.
  cypressLoader defaultSymbolDictionaryName: self defaultSymbolDictionaryName.
  self resolvedPackageReferences
    do: [ :resolvedReference | | package repository snapshot |
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

{ #category : 'Accessing' }
CypressPackageManager3 >> resolvedPackageReferences [
  ^ resolvedPackageReferences

]

{ #category : 'Accessing' }
CypressPackageManager3 >> resolvedPackageReferences: anObject [
  resolvedPackageReferences := anObject

]

{ #category : 'Unloading' }
CypressPackageManager3 >> unloadPackage: aPackage [

  | loader summary |
  loader := (CypressPackageDefinition named: aPackage name) snapshot
              unload.
  summary := Dictionary new.
  loader unloadable notEmpty
    ifTrue: [ 
      summary
        at: 'Unloadable'
        put: (loader unloadable collect: [ :each | each printString ]) ].
  loader errors notEmpty
    ifTrue: [ summary at: 'Errors' put: (loader errors collect: [ :each | each printString ]) ].
  loader requirements notEmpty
    ifTrue: [ summary at: 'Missing Requirements' put: loader requirements asArray ].
  ^ summary

]

{ #category : 'Unloading' }
CypressPackageManager3 >> unloadPackageNamed: aPackageName [
  ^ self unloadPackage: (CypressPackageDefinition named: aPackageName)

]
