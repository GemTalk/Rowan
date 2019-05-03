Class {
	#name : 'RwLoadedProject',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedPackages',
		'isDirty'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwLoadedProject class >> _projectDefinitionSourceKey [
	"The value of the property key indicates which source the project definition was derived from.
		Currently used when deciding whether to change the loaded commit id, during a load ... 
		if the load is derived from a loaded project (RwLoadedProject class _projectDefinitionSourceValue), 
		then it isn't necessary to update the loaded commit id."

	^ '_Project_Definition_Source'
]

{ #category : 'accessing' }
RwLoadedProject class >> _projectDiskDefinitionSourceValue [
	"This value of the property key indicates that the source of the project definition was loaded from disk, 
		then it isn't necessary to update the loaded commit id."

	^ 'loaded from disk'
]

{ #category : 'accessing' }
RwLoadedProject class >> _projectLoadedDefinitionSourceValue [
	"This value of the property key indicates that the source of the project definition was derived from
		a loaded project, then it isn't necessary to update the loaded commit id."

	^ 'loaded project'
]

{ #category : 'accessing' }
RwLoadedProject class >> _projectUnknownDefinitionSourceValue [
	"This value of the property key indicates that the source of the project definition is unknown."

	^ 'unknown'
]

{ #category : 'instance creation' }
RwLoadedProject class >> newForLoadSpecification: aLoadSpecification [

	^ self new
		initialize;
		initializeForLoadSpecification: aLoadSpecification;
		yourself
]

{ #category : 'accessing' }
RwLoadedProject >> addLoadedPackage: aLoadedPackage [

	self markDirty.
	loadedPackages at: aLoadedPackage key put: aLoadedPackage.
	aLoadedPackage loadedProject: self
]

{ #category : 'definitions' }
RwLoadedProject >> asDefinition [

	"Create a Definition corresponding to what I define in the image."

	^ RwProjectDefinition
		withProperties: self propertiesForDefinition
		packageDefinitions: self loadedPackageDefinitions
]

{ #category : 'initialization' }
RwLoadedProject >> initialize [

	super initialize.
	loadedPackages := KeyValueDictionary new.
	isDirty := true. "a project is dirty if it has changes that are not written to disk"

]

{ #category : 'initialization' }
RwLoadedProject >> initializeForLoadSpecification: aLoadSpecification [

	self initializeForName: aLoadSpecification specName.
	handle := aLoadSpecification
]

{ #category : 'testing' }
RwLoadedProject >> isDirty [

	"a project is dirty if it has changes that are not written to disk, or it's packages have changes that are not written to  disk."

	isDirty ifTrue: [ ^ true].
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

	^ handle
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

{ #category : 'private' }
RwLoadedProject >> markProjectDirty [

	self markDirty
]

{ #category : 'accessing' }
RwLoadedProject >> packageNames [

	"Return list of package names in the receiver"

	^ self loadedPackages keys asArray
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
	props at: 'name' put: name.
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
