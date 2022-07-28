Class {
	#name : 'RwDefinedFromResolvedProject',
	#superclass : 'RwDefinedProject',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwDefinedFromResolvedProject class >> fromLoadedProject: aLoadedProject [
	^ (self newNamed: aLoadedProject name)
		_concreteProject: aLoadedProject _concreteProject asDefinition;
		yourself
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> _projectDefinitionPlatformConditionalAttributes [
	"Answer theplatformConditionalAttributes that will be used to load the project"

	^ self _concreteProject _projectDefinitionPlatformConditionalAttributes
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> componentsRoot [
	^ self _concreteProject componentsRoot
]

{ #category : 'actions' }
RwDefinedFromResolvedProject >> export [
	^ self _concreteProject export
]

{ #category : 'actions' }
RwDefinedFromResolvedProject >> exportComponents [
	^ self _concreteProject exportComponents
]

{ #category : 'actions' }
RwDefinedFromResolvedProject >> exportLoadSpecification [
	^ self _concreteProject exportLoadSpecification
]

{ #category : 'actions' }
RwDefinedFromResolvedProject >> exportPackages [
	^ self _concreteProject exportPackages
]

{ #category : 'actions' }
RwDefinedFromResolvedProject >> exportPackages: diskProjectSetDefinition packagesRoot: packagesRoot packageFormat: packageFormat packageConvention: packageConvention [
	^ self _concreteProject
		exportPackages: diskProjectSetDefinition
		packagesRoot: packagesRoot
		packageFormat: packageFormat
		packageConvention: packageConvention
]

{ #category : 'actions' }
RwDefinedFromResolvedProject >> exportProjects [
	^ self _concreteProject exportProjects
]

{ #category : 'actions' }
RwDefinedFromResolvedProject >> exportProjectSpecification [
	^ self _concreteProject exportProjectSpecification
]

{ #category : 'actions' }
RwDefinedFromResolvedProject >> exportTopazFormatTo: filePath [
	^ self
		exportTopazFormatTo: filePath
		logClassCreation: false
		excludeClassInitializers: false
		excludeRemoveAllMethods: false
]

{ #category : 'actions' }
RwDefinedFromResolvedProject >> exportTopazFormatTo: filePath logClassCreation: logClassCreation excludeClassInitializers: excludeClassInitializers excludeRemoveAllMethods: excludeRemoveAllMethods [
	^ self _concreteProject
		exportTopazFormatTo: filePath
		logClassCreation: logClassCreation
		excludeClassInitializers: excludeClassInitializers
		excludeRemoveAllMethods: excludeRemoveAllMethods
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> loadProjectSet [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.
	"

	^ self _concreteProject loadProjectSet
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> packagesRoot [
	^ self _concreteProject packagesRoot
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> projectRoots [
	^ self _concreteProject projectRoots
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> projectsRoot [
	^ self _concreteProject projectsRoot
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> read [
	"return a RwDefinedProject with definitions read from disk"

	self _concreteProject read
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> read: platformConditionalAttributes [
	"return a RwDefinedProject with definitions read from disk ... the reciever will match the definitions on disk based on the current load specification"

	self _concreteProject read: platformConditionalAttributes
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> readProjectComponentNames: componentNames [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self _concreteProject readProjectComponentNames: componentNames
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> readProjectComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self _concreteProject
		readProjectComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> readProjectSet [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	self _concreteProject readProjectSet
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> readProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self _concreteProject readProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> repository [
	^ self _concreteProject repository
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> repositoryRoot [
	^ self _concreteProject repositoryRoot
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> revision [
	^ self _concreteProject revision
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> revision: aString [
	self _concreteProject revision: aString
]

{ #category : 'accessing' }
RwDefinedFromResolvedProject >> specsRoot [
	^ self _concreteProject specsRoot
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> write [

	self _concreteProject
		export;
		exportLoadSpecification
]
