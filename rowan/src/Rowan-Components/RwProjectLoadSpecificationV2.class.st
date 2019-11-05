"
I am used to specify all of the properties that are used to load a project from disk.
"
Class {
	#name : 'RwProjectLoadSpecificationV2',
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

{ #category : 'instance creation' }
RwProjectLoadSpecificationV2 class >> specName: specName projectName: projectName comment: commentString [
	^ self new
		specName: projectName;
		projectName: projectName;
		comment: commentString;
		yourself
]

{ #category : 'converting' }
RwProjectLoadSpecificationV2 >> asProjectDefiniton [
	^ RwProjectDefinitionV2 fromLoadSpecification: self
]

{ #category : 'converting' }
RwProjectLoadSpecificationV2 >> asProjectDefinitonWithComment: commentString [
	^ (RwProjectDefinitionV2 fromLoadSpecification: self)
		comment: commentString;
		yourself
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> comment [
	^ comment ifNil: [ ^ '' ]
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> commitish [
	^ commitish
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> commitish: aString [
	commitish := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> componentNames [
	^ componentNames
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> componentNames: anArray [
	componentNames := anArray asArray
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> componentsPath [

	^ componentsPath
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> componentsPath: aString [
	componentsPath := aString
]

{ #category : 'private' }
RwProjectLoadSpecificationV2 >> currentVersion [
	"
		0.1.0 - first class load specification
	"
	^ '0.1.0'
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> defaultSymbolDictName [

	^ defaultSymbolDictName
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> defaultSymbolDictName: aString [
	defaultSymbolDictName := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> defaultUseSessionMethodsForExtensions [
	^ defaultUseSessionMethodsForExtensions ifNil: [ ^ false ]
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> defaultUseSessionMethodsForExtensions: aString [
	defaultUseSessionMethodsForExtensions := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> groupNames [
	^ groupNames
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> groupNames: anArray [
	groupNames := anArray asArray
]

{ #category : 'initialization' }
RwProjectLoadSpecificationV2 >> initialize [
	super initialize.
	commitish := 'master'.
	componentNames := #('Core').
	componentsPath := 'rowan/components'.
	defaultSymbolDictName := 'UserGlobals'.
	groupNames := #('core' 'tests').
	packagesPath := 'rowan/src'.
	projectsPath := 'rowan/projects'.
	specsPath := 'rowan/specs'.
	projectName := 'ProjectName'
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> packagesPath [

	^ packagesPath
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> packagesPath: aString [
	packagesPath := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectAlias [
	^ projectAlias ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectAlias: aString [
	projectAlias := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectName [

	^ projectName
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectName: aString [
	projectName := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectsPath [

	^ projectsPath
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectsPath: aString [
	projectsPath := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectUrl [
	^ projectUrl ifNil: [ '' ]
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> projectUrl: aString [
	projectUrl := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> specName [

	^ specName
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> specName: aString [
	specName := aString
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> specsPath [

	^ specsPath
]

{ #category : 'accessing' }
RwProjectLoadSpecificationV2 >> specsPath: aString [
	specsPath := aString
]
