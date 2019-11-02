"
I am used to specify all of the properties that are used to load a project from disk.
"
Class {
	#name : 'RwProjectLoadSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'specName',
		'projectName',
		'projectAlias',
		'projectsPath',
		'projectUrl',
		'commitish',
		'componentsPath',
		'specsPath',
		'packagesPath',
		'comment',
		'componentNames',
		'groupNames',
		'defaultSymbolDictName',
		'defaultUseSessionMethodsForExtensions'
	],
	#category : 'Rowan-Components'
}

{ #category : 'accessing' }
RwProjectLoadSpecification >> comment [
	^ comment ifNil: [ ^ '' ]
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> commitish [
	^ commitish
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> commitish: aString [
	commitish := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> componentNames [
	^ componentNames
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> componentNames: anArray [
	componentNames := anArray asArray
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> componentsPath [

	^ componentsPath
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> componentsPath: aString [
	componentsPath := aString
]

{ #category : 'private' }
RwProjectLoadSpecification >> currentVersion [
	"
		0.1.0 - first class load specification
	"
	^ '0.1.0'
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> defaultSymbolDictName [

	^ defaultSymbolDictName
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> defaultSymbolDictName: aString [
	defaultSymbolDictName := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> defaultUseSessionMethodsForExtensions [
	^ defaultUseSessionMethodsForExtensions ifNil: [ ^ false ]
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> defaultUseSessionMethodsForExtensions: aString [
	defaultUseSessionMethodsForExtensions := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> groupNames [
	^ groupNames
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> groupNames: anArray [
	groupNames := anArray asArray
]

{ #category : 'initialization' }
RwProjectLoadSpecification >> initialize [
	super initialize.
	commitish := 'master'.
	componentNames := #('Core').
	componentsPath := 'rowan/components'.
	defaultSymbolDictName := 'UserGlobals'.
	groupNames := #('core').
	packagesPath := 'rowan/src'.
	projectsPath := 'rowan/projects'.
	specsPath := 'rowan/specs'.
projectName := 'ProjectName'.
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> packagesPath [

	^ packagesPath
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> packagesPath: aString [
	packagesPath := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectAlias [
	^ projectAlias ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectAlias: aString [
	projectAlias := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectName [

	^ projectName
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectName: aString [
	projectName := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectsPath [

	^ projectsPath
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectsPath: aString [
	projectsPath := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectUrl [
	^ projectUrl ifNil: [ '' ]
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectUrl: aString [
	projectUrl := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> specName [

	^ specName
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> specName: aString [
	specName := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> specsPath [

	^ specsPath
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> specsPath: aString [
	specsPath := aString
]
