Class {
	#name : 'RwLoadedProject',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedPackages',
		'isDirty'
	],
	#category : 'Rowan-Loader'
}

{ #category : 'accessing' }
RwLoadedProject class >> _projectDefinitionPlatformConditionalAttributesKey [
	"The value of the property key preserves the platform conditional attributes used to read the project from disk"

	^ #'_Project_Definition_PlatformConditionalAttributes'
]

{ #category : 'accessing' }
RwLoadedProject class >> _projectDefinitionSourceKey [
	"The value of the property key indicates which source the project definition was derived from.
		Currently used when deciding whether to change the loaded commit id, during a load ... 
		if the load is derived from a loaded project (RwLoadedProject class _projectDefinitionSourceValue), 
		then it isn't necessary to update the loaded commit id."

	^ #'_Project_Definition_Source'
]

{ #category : 'accessing' }
RwLoadedProject class >> _projectDiskDefinitionSourceValue [
	"This value of the property key indicates that the source of the project definition was loaded from disk, 
		then it isn't necessary to update the loaded commit id."

	^ #'loaded from disk'
]

{ #category : 'accessing' }
RwLoadedProject class >> _projectLoadedDefinitionSourceValue [
	"This value of the property key indicates that the source of the project definition was derived from
		a loaded project, then it isn't necessary to update the loaded commit id."

	^ #'loaded project'
]

{ #category : 'accessing' }
RwLoadedProject class >> _projectLoadedDefinitionSourceWithDependentProjectsValue [
	"This value of the property key indicates that the source of the project definition was loaded from disk, 
		however, the project has dependent projects that will need to be reread from disk on load."

	^ #'loaded from disk with dependent projects'
]

{ #category : 'accessing' }
RwLoadedProject class >> _projectModifiedProjectSourceValue [
	"This value of the property key indicates that the source of the project definition is newly created project or explicitly modified, so we don't want to read project from disk on load."

	^ #'modified project'
]

{ #category : 'accessing' }
RwLoadedProject class >> _projectUnknownDefinitionSourceValue [
	"This value of the property key indicates that the source of the project definition is unknown."

	^ #'unknown'
]

{ #category : 'comparing' }
RwLoadedProject >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [
	({#'projectOwnerId'.
	#'spec'.
	(RwLoadedProject _projectDefinitionSourceKey).
	(RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey).
	#'projectRef' } includesIdentical: propertyKey)
		ifTrue: [ 
			"projectRef entries are considered to be equal for comparison purposes"
			"spec entries are considered to be equal for comparison purposes"
			"_projectDefinitionSourceKey entries are considered equal for comparison purpposes"
			"_projectDefinitionPlatformConditionalAttributesKey entries are considered equal for comparison purpposes"
			"projectOwnerId entries are considered equal for comparison purpposes"
			^ true ].
	^ super
		_compareProperty: propertyKey
		propertyVaue: propertyValue
		againstBaseValue: baseValue
]

{ #category : 'accessing' }
RwLoadedProject >> addLoadedPackage: aLoadedPackage [

	self markDirty.
	loadedPackages at: aLoadedPackage key put: aLoadedPackage.
	aLoadedPackage loadedProject: self
]

{ #category : 'actions' }
RwLoadedProject >> audit [
	"run audit on the receiver"

	^ Rowan projectTools audit auditForProject: self
]

{ #category : 'actions' }
RwLoadedProject >> auditOn: logStreamOrNil [
	"run audit on the receiver"

	^ Rowan projectTools audit auditForProject: self on: logStreamOrNil
]

{ #category : 'commit log' }
RwLoadedProject >> commitLog: logLimit [

	self subclassResponsibility: #commitLog:
]

{ #category : 'comparing' }
RwLoadedProject >> compareAgainstBase: aDefinition [

	| modification packagesModification |
	modification := RwProjectModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	packagesModification := RwPackagesModification new.
	self
		compareDictionary: self packagesForCompare
		againstBaseDictionary: aDefinition packagesForCompare
		into: packagesModification
		elementClass: RwPackageDefinition.
	modification packagesModification: packagesModification.
	^ modification
]

{ #category : 'initialization' }
RwLoadedProject >> initialize [

	super initialize.
	loadedPackages := KeyValueDictionary new.
	isDirty := true. "a project is dirty if it has changes that are not written to disk"

]

{ #category : 'testing' }
RwLoadedProject >> isDirty [
	"a project is dirty if it has changes that are not written to disk, or it's packages have changes that are not written to  disk."

	isDirty
		ifTrue: [ ^ true ].
	self loadedPackages
		do: [ :loadedPackage | 
			loadedPackage isDirty
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'testing' }
RwLoadedProject >> isEmpty [

	^ loadedPackages isEmpty
]

{ #category : 'accessing' }
RwLoadedProject >> key [

	^ name
]

{ #category : 'actions' }
RwLoadedProject >> load [
	"load the receiver into the image"

	| projectDefinition projectSetDefinition readProjectDefinition |
	projectDefinition := self asDefinition.
	projectSetDefinition := projectDefinition read.
	readProjectDefinition := projectSetDefinition projectNamed: projectDefinition name.
	readProjectDefinition == projectDefinition 
		ifFalse: [
			"https://github.com/GemTalk/Rowan/issues/488"
			self error: 'expected to update the projectDefinition in-place' ].
	^ Rowan projectTools loadV2 loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'actions' }
RwLoadedProject >> load: instanceMigrator [
	"load the receiver into the image"

	| projectDefinition projectSetDefinition readProjectDefinition |
	projectDefinition := self asDefinition.
	projectSetDefinition := projectDefinition read.
	readProjectDefinition := projectSetDefinition projectNamed: projectDefinition name.
	readProjectDefinition == projectDefinition 
		ifFalse: [
			"https://github.com/GemTalk/Rowan/issues/488"
			self halt: 'expected to update the projectDefinition in-place' ].
	^ Rowan projectTools loadV2 loadProjectSetDefinition: projectSetDefinition instanceMigrator: instanceMigrator
]

{ #category : 'accessing' }
RwLoadedProject >> loadedCommitId [

	self subclassResponsibility: #loadedCommitId
]

{ #category : 'accessing' }
RwLoadedProject >> loadedPackageDefinitions [

	"Create definitions from all of the packages I define, and answer the collection of them"

	^ self definitionsFor: loadedPackages
]

{ #category : 'accessing' }
RwLoadedProject >> loadedPackages [

	^ loadedPackages
]

{ #category : 'accessing' }
RwLoadedProject >> loadedPackages: anObject [

   loadedPackages := anObject

]

{ #category : 'enumeration' }
RwLoadedProject >> loadedPackagesDo: loadedPackageBlock loadedClassedDo: loadedClassBlock loadedClassExtensionsDo: loadedClassExtensionBlock loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock [

	loadedPackages
		valuesDo: [ :loadedPackage | 
			loadedPackageBlock cull: self cull: loadedPackage.
			loadedPackage
				loadedClassedDo: loadedClassBlock
				loadedClassExtensionsDo: loadedClassExtensionBlock
				loadedInstanceMethodsDo: loadedInstanceMethodBlock
				loadedClassMethodsDo: loadedClassMethodBlock ]
]

{ #category : 'accessing' }
RwLoadedProject >> loadedProject [

	^ self
]

{ #category : 'accessing' }
RwLoadedProject >> loadSpecification [

	self subclassResponsibility: #loadSpecification
]

{ #category : 'private' }
RwLoadedProject >> markDirty [
	"I have been changed independent of a change to my repository, and therefore may be out of sync with my repository"

	isDirty := true
]

{ #category : 'private' }
RwLoadedProject >> markNotDirty [
	"I have been changed to be consistent with my source, so mark not dirty."

	isDirty := false
]

{ #category : 'accessing' }
RwLoadedProject >> packageNames [

	"Return list of package names in the receiver"

	^ self loadedPackages keys asArray
]

{ #category : 'accessing' }
RwLoadedProject >> prettyName [

	^' project ' , self name
]

{ #category : 'accessing' }
RwLoadedProject >> projectDefinitionSourceProperty [

	^ properties at: RwLoadedProject _projectDefinitionSourceKey ifAbsent: [ RwLoadedProject _projectUnknownDefinitionSourceValue ]
]

{ #category : 'accessing' }
RwLoadedProject >> projectUrl [

	"Return the projectUrl used to clone the project"

	^ self specification projectUrl
]

{ #category : 'definitions' }
RwLoadedProject >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: #'name' put: name.
	props at: self class _projectDefinitionSourceKey put: self class _projectLoadedDefinitionSourceValue.
	^ props
]

{ #category : 'accessing' }
RwLoadedProject >> removeLoadedPackage: aLoadedPackage [

	self markDirty.
	loadedPackages removeKey: aLoadedPackage key
]

{ #category : 'properties' }
RwLoadedProject >> specification [

	^ properties at: 'spec'
]

{ #category : 'actions' }
RwLoadedProject >> unload [
	"unload the receiver from the image"

	| projectDefinition |
	projectDefinition := self asDefinition.
	^ Rowan projectTools delete deleteProjectDefinition: projectDefinition
]

{ #category : 'testing' }
RwLoadedProject >> useGit [

	^ self subclassResponsibility: #useGit
]
