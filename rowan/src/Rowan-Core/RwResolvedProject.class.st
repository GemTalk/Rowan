Class {
	#name : 'RwResolvedProject',
	#superclass : 'RwAbstractUnloadedProject',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwResolvedProject class >> fromDefinedProject: aDefinedProject [
	^ (self newNamed: aDefinedProject name)
		_resolvedProject: aDefinedProject _resolvedProject resolve;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> fromStrictDefinedProject: aDefinedProject [
	| createBlock oldPolicy |
	createBlock := [ 
	(self newNamed: aDefinedProject name)
		_resolvedProject: aDefinedProject _resolvedProject resolve;
		yourself ].
	aDefinedProject isStrict
		ifTrue: [ ^ createBlock value ].
	oldPolicy := aDefinedProject repositoryResolutionPolicy.
	[ 
	aDefinedProject repositoryResolutionPolicy: #'strict'.
	^ createBlock value ]
		ensure: [ aDefinedProject repositoryResolutionPolicy: oldPolicy ]
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl [
	| loadSpec resolvedProject |
	loadSpec := RwSpecification fromUrl: loadSpecUrl.
	resolvedProject := loadSpec resolve.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		yourself.
	resolvedProject := loadSpec resolve.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		componentNames: componentNames;
		yourself.
	resolvedProject := loadSpec resolve.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames customConditionalAttributes: customConditionalAttributes [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		componentNames: componentNames;
		customConditionalAttributes: customConditionalAttributes;
		yourself.
	resolvedProject := loadSpec resolve.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		componentNames: componentNames;
		yourself.
	resolvedProject := loadSpec resolve: platformConditionalAttributes.
	^ (self newNamed: resolvedProject name)
		_resolvedProject: resolvedProject;
		yourself
]

{ #category : 'transitions' }
RwResolvedProject >> defined [
	^ RwDefinedProject fromResolvedProject: self
]

{ #category : 'actions' }
RwResolvedProject >> export [
	^ self _resolvedProject export
]

{ #category : 'actions' }
RwResolvedProject >> exportComponents [
	^ self _resolvedProject exportComponents
]

{ #category : 'actions' }
RwResolvedProject >> exportLoadSpecification [
	^ self _resolvedProject exportLoadSpecification
]

{ #category : 'actions' }
RwResolvedProject >> exportPackages [
	^ self _resolvedProject exportPackages
]

{ #category : 'actions' }
RwResolvedProject >> exportPackages: diskProjectSetDefinition packagesRoot: packagesRoot packageFormat: packageFormat packageConvention: packageConvention [
	^ self _resolvedProject
		exportPackages: diskProjectSetDefinition
		packagesRoot: packagesRoot
		packageFormat: packageFormat
		packageConvention: packageConvention
]

{ #category : 'actions' }
RwResolvedProject >> exportProjectSpecification [
	^ self _resolvedProject exportProjectSpecification
]

{ #category : 'actions' }
RwResolvedProject >> exportTopazFormatTo: filePath [
	^ self _resolvedProject exportTopazFormatTo: filePath
]

{ #category : 'actions' }
RwResolvedProject >> exportTopazFormatTo: filePath usingPackageNamesMap: packageNamesMap [
	^ self _resolvedProject exportTopazFormatTo: filePath usingPackageNamesMap: packageNamesMap
]

{ #category : 'accessing' }
RwResolvedProject >> projectRoots [
	^ self _resolvedProject projectRoots
]

{ #category : 'accessing' }
RwResolvedProject >> repository [
	^ self _resolvedProject repository
]

{ #category : 'accessing' }
RwResolvedProject >> repositoryRoot [
	^ self _resolvedProject repositoryRoot
]

{ #category : 'accessing' }
RwResolvedProject >> revision [
	^ self _resolvedProject revision
]

{ #category : 'accessing' }
RwResolvedProject >> revision: aString [
	self _resolvedProject revision: aString
]

{ #category : 'accessing' }
RwResolvedProject >> specsRoot [
	^ self _resolvedProject specsRoot
]

{ #category : 'transitions' }
RwResolvedProject >> write [

	self _resolvedProject
		export;
		exportLoadSpecification
]
