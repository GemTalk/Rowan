Class {
	#name : 'RwResolvedProject',
	#superclass : 'RwAbstractUnloadedProject',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwResolvedProject class >> fromDefinedProject: aDefinedProject [
	| theProject |
	theProject :=  aDefinedProject _concreteProject.
	theProject _basicResolve.
	^ (self newNamed: aDefinedProject name)
		_concreteProject: theProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> fromStrictDefinedProject: aDefinedProject [
	| createBlock oldPolicy |
	createBlock := [ 
	(self newNamed: aDefinedProject name)
		_concreteProject: aDefinedProject _concreteProject resolveProject;
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
	resolvedProject := loadSpec resolveProject.
	^ (self newNamed: resolvedProject name)
		_concreteProject: resolvedProject resolveProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl diskUrl: urlString [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		diskUrl: urlString;
		projectsHome: urlString asRwUrl pathString asFileReference parent;
		yourself.
	resolvedProject := loadSpec resolveProject.
	^ (self newNamed: resolvedProject name)
		_concreteProject: resolvedProject resolveProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl gitUrl: urlString [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		gitUrl: urlString;
		projectsHome: urlString asRwUrl pathString asFileReference parent;
		yourself.
	resolvedProject := loadSpec resolveProject.
	^ (self newNamed: resolvedProject name)
		_concreteProject: resolvedProject resolveProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		yourself.
	resolvedProject := loadSpec resolveProject.
	^ (self newNamed: resolvedProject name)
		_concreteProject: resolvedProject resolveProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		componentNames: componentNames;
		yourself.
	resolvedProject := loadSpec resolveProject.
	^ (self newNamed: resolvedProject name)
		_concreteProject: resolvedProject resolveProject;
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
	resolvedProject := loadSpec resolveProject.
	^ (self newNamed: resolvedProject name)
		_concreteProject: resolvedProject resolveProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome customConditionalAttributes: customConditionalAttributes [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		customConditionalAttributes: customConditionalAttributes;
		yourself.
	resolvedProject := loadSpec resolveProject.
	^ (self newNamed: resolvedProject name)
		_concreteProject: resolvedProject resolveProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		projectsHome: projectsHome;
		yourself.
	loadSpec addCustomConditionalAttributes: customConditionalAttributes.
	resolvedProject := loadSpec resolveProject.
	resolvedProject read: platformConditionalAttributes.
	^ (self newNamed: resolvedProject name)
		_concreteProject: resolvedProject resolveProject;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProject class >> projectFromUrl: loadSpecUrl readonlyDiskUrl: urlString [
	| loadSpec resolvedProject |
	loadSpec := (RwSpecification fromUrl: loadSpecUrl)
		readonlyDiskUrl: urlString;
		projectsHome: urlString asRwUrl pathString asFileReference parent;
		yourself.
	resolvedProject := loadSpec resolveProject.
	^ (self newNamed: resolvedProject name)
		_concreteProject: resolvedProject resolveProject;
		yourself
]

{ #category : 'accessing' }
RwResolvedProject >> _projectDefinitionPlatformConditionalAttributes [
	"Answer theplatformConditionalAttributes that will be used to load the project"

	^ self _concreteProject _projectDefinitionPlatformConditionalAttributes
]

{ #category : 'testing' }
RwResolvedProject >> canCommit [
	^ self _concreteProject canCommit
]

{ #category : 'actions' }
RwResolvedProject >> checkout: revision [
	^ self _concreteProject checkout: revision
]

{ #category : 'actions' }
RwResolvedProject >> commit: message [
	"commit the repository associated with receiver ..."

	^ self _concreteProject commit: message
]

{ #category : 'querying' }
RwResolvedProject >> commitId [

	^ self _concreteProject commitId
]

{ #category : 'querying' }
RwResolvedProject >> commitLog: logLimit [
	^ self _concreteProject commitLog: logLimit
]

{ #category : 'accessing' }
RwResolvedProject >> componentsRoot [
	^ self _concreteProject componentsRoot
]

{ #category : 'transitions' }
RwResolvedProject >> defined [
	^ RwDefinedFromResolvedProject fromLoadedProject: self
]

{ #category : 'actions' }
RwResolvedProject >> export [
	^ self _concreteProject export
]

{ #category : 'actions' }
RwResolvedProject >> exportComponents [
	^ self _concreteProject exportComponents
]

{ #category : 'actions' }
RwResolvedProject >> exportLoadSpecification [
	^ self _concreteProject exportLoadSpecification
]

{ #category : 'actions' }
RwResolvedProject >> exportPackages [
	^ self _concreteProject exportPackages
]

{ #category : 'actions' }
RwResolvedProject >> exportPackages: diskProjectSetDefinition packagesRoot: packagesRoot packageFormat: packageFormat packageConvention: packageConvention [
	^ self _concreteProject
		exportPackages: diskProjectSetDefinition
		packagesRoot: packagesRoot
		packageFormat: packageFormat
		packageConvention: packageConvention
]

{ #category : 'actions' }
RwResolvedProject >> exportProjects [
	^ self _concreteProject exportProjects
]

{ #category : 'actions' }
RwResolvedProject >> exportProjectSpecification [
	^ self _concreteProject exportProjectSpecification
]

{ #category : 'actions' }
RwResolvedProject >> exportTopazFormatTo: filePath [
	^ self
		exportTopazFormatTo: filePath
		logClassCreation: false
		excludeClassInitializers: false
		excludeRemoveAllMethods: false
]

{ #category : 'actions' }
RwResolvedProject >> exportTopazFormatTo: filePath logClassCreation: logClassCreation excludeClassInitializers: excludeClassInitializers excludeRemoveAllMethods: excludeRemoveAllMethods [
	^ self _concreteProject
		exportTopazFormatTo: filePath
		logClassCreation: logClassCreation
		excludeClassInitializers: excludeClassInitializers
		excludeRemoveAllMethods: excludeRemoveAllMethods
]

{ #category : 'accessing' }
RwResolvedProject >> packagesRoot [
	^ self _concreteProject packagesRoot
]

{ #category : 'accessing' }
RwResolvedProject >> projectRoots [
	^ self _concreteProject projectRoots
]

{ #category : 'accessing' }
RwResolvedProject >> projectsRoot [
	^ self _concreteProject projectsRoot
]

{ #category : 'transitions' }
RwResolvedProject >> read [
	"return a RwDefinedProject with definitions read from disk"

	self _concreteProject read
]

{ #category : 'transitions' }
RwResolvedProject >> read: platformConditionalAttributes [
	"return a RwDefinedProject with definitions read from disk ... the reciever will match the definitions on disk based on the current load specification"

	self _concreteProject read: platformConditionalAttributes
]

{ #category : 'actions' }
RwResolvedProject >> readPackageNames: packageNames [
	^ self _concreteProject readPackageNames: packageNames
]

{ #category : 'actions' }
RwResolvedProject >> readPackageNamesBlock: packageNamesBlock [
	^ self _concreteProject readPackageNamesBlock: packageNamesBlock
]

{ #category : 'transitions' }
RwResolvedProject >> readProjectComponentNames: componentNames [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self _concreteProject readProjectComponentNames: componentNames
]

{ #category : 'transitions' }
RwResolvedProject >> readProjectComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self _concreteProject
		readProjectComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'transitions' }
RwResolvedProject >> readProjectSet [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	self _concreteProject readProjectSet
]

{ #category : 'actions' }
RwResolvedProject >> readProjectSet: customConditionalAttributes [
	"refresh the contents of the receiver ... use customConditionalAttributes to determine which components will be loaded"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	self _concreteProject readProjectSet: customConditionalAttributes
]

{ #category : 'transitions' }
RwResolvedProject >> readProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self _concreteProject readProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'accessing' }
RwResolvedProject >> repository [
	^ self _concreteProject repository
]

{ #category : 'accessing' }
RwResolvedProject >> repositoryRoot [
	^ self _concreteProject repositoryRoot
]

{ #category : 'transitions' }
RwResolvedProject >> resolveProject [
	^ self
]

{ #category : 'accessing' }
RwResolvedProject >> revision [
	^ self _concreteProject revision
]

{ #category : 'accessing' }
RwResolvedProject >> revision: aString [
	self _concreteProject revision: aString
]

{ #category : 'accessing' }
RwResolvedProject >> specsRoot [
	^ self _concreteProject specsRoot
]

{ #category : 'transitions' }
RwResolvedProject >> write [

	self _concreteProject
		export;
		exportLoadSpecification
]
