Class {
	#name : 'RwAbstractUnloadedProject',
	#superclass : 'RwAbstractProject',
	#instVars : [
		'concreteProject'
	],
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwAbstractUnloadedProject >> _concreteProject [
	^ concreteProject
]

{ #category : 'private' }
RwAbstractUnloadedProject >> _concreteProject: aResolvedProject [
	concreteProject := aResolvedProject
]

{ #category : 'private' }
RwAbstractUnloadedProject >> _projectComponents [
	^ self _concreteProject _projectComponents
]

{ #category : 'private' }
RwAbstractUnloadedProject >> _projectDefinition [
	^ self _concreteProject _projectDefinition
]

{ #category : 'private' }
RwAbstractUnloadedProject >> _projectRepository [
	^ self _concreteProject _projectRepository
]

{ #category : 'private' }
RwAbstractUnloadedProject >> _projectSpecification [
	^ self _concreteProject _projectSpecification
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> comment: aString [
	self _concreteProject comment: aString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> diskUrl: aString [
	self loadSpecification diskUrl: aString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultMethodEnvForUser: userId to: env [
	self _concreteProject gemstoneSetDefaultMethodEnvForUser: userId to: env
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultMethodEnvTo: env [
	self _concreteProject gemstoneSetDefaultMethodEnvTo: env
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName [
	self _concreteProject gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self _concreteProject gemstoneSetDefaultSymbolDictNameTo: symbolDictName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId to: aBool [
	self _concreteProject gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId to: aBool
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool [
	self _concreteProject gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetMethodEnv: env forPackageNamed: packageName [
	self _concreteProject gemstoneSetMethodEnv: env forPackageNamed: packageName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetMethodEnvForUser: userId to: env forPackageNamed: packageName [
	self _concreteProject
		gemstoneSetMethodEnvForUser: userId
		to: env
		forPackageNamed: packageName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	self _concreteProject
		gemstoneSetSymbolDictName: symbolDictName
		forPackageNamed: packageName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetSymbolDictNameForUser: userId to: symbolDictName forPackageNamed: packageName [
	self _concreteProject
		gemstoneSetSymbolDictNameForUser: userId
		to: symbolDictName
		forPackageNamed: packageName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetUseSessionMethodsForExtensions: userId to: aBool [
	self _concreteProject
		gemstoneSetUseSessionMethodsForExtensions: userId
		to: aBool
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetUseSessionMethodsForExtensionsForUser: userId to: aBool forPackageNamed: packageName [
	self _concreteProject
		gemstoneSetUseSessionMethodsForExtensionsForUser: userId
		to: aBool
		forPackageNamed: packageName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gitUrl: aString [
	self _concreteProject gitUrl: aString
]

{ #category : 'transitions' }
RwAbstractUnloadedProject >> load [
	"
		load only the receiver into the image. Required projects for the receiver are only loaded if they are 
			not already present in the image.

		To explicitly load the receiver AND required projects, construct a project set containing projects to 
			be loaded and send #load to the project set.
	"

	^ self _concreteProject load
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> packageFormat: aString [
	^ self _concreteProject packageFormat: aString
]

{ #category : 'actions' }
RwAbstractUnloadedProject >> packages [
	^ self _concreteProject packages
]

{ #category : 'actions' }
RwAbstractUnloadedProject >> packages: aPackageDictionary [
	self _concreteProject packages: aPackageDictionary
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> projectVersion: aStringOrVersion [
	^ self _concreteProject projectVersion: aStringOrVersion
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> relativeRepositoryRoot: aRelativePathString [
	"specify the repository root relative to the git repository root ... not applicable to non-git (svn, etc.) repositories"

	self loadSpecification relativeRepositoryRoot: aRelativePathString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> repositoryResolutionPolicy [
	^ self loadSpecification repositoryResolutionPolicy
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> repositoryResolutionPolicy: aSymbolOrNil [
	self loadSpecification repositoryResolutionPolicy: aSymbolOrNil
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> specComponentNames: anArray [
	self loadSpecification componentNames: anArray
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> specName [
	^ self loadSpecification specName
]

{ #category : 'querying' }
RwAbstractUnloadedProject >> subcomponentsOf: componentName [
	"list of direct subcomponents of the given <componentName> ...includes package groups"

	^ self subcomponentsOf: componentName ifNone: [ ^ {} ]
]

{ #category : 'querying' }
RwAbstractUnloadedProject >> subcomponentsOf: componentName attributes: attributes ifNone: noneBlock [
	| subcomponents |
	subcomponents := self _projectComponents
		subcomponentsOf: componentName
		matchBlock: [ :aComponent | aComponent matchesAttributes: attributes ]
		ifNone: [ ^ noneBlock value ].
	subcomponents isEmpty
		ifTrue: [ ^ noneBlock value ].
	^ subcomponents
]

{ #category : 'querying' }
RwAbstractUnloadedProject >> subcomponentsOf: componentName ifNone: noneBlock [
	"list of direct subcomponents of the given <componentName> ...includes package groups"

	| subcomponents |
	subcomponents := self _projectComponents
		subcomponentsOf: componentName
		matchBlock: [ :aComponent | 
			"match all components"
			true ]
		ifNone: [ ^ noneBlock value ].
	subcomponents isEmpty
		ifTrue: [ ^ noneBlock value ].
	^ subcomponents
]

{ #category : 'testing' }
RwAbstractUnloadedProject >> useGit [

	^self _concreteProject useGit
]
