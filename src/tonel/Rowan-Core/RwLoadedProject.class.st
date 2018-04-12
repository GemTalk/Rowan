Class {
	#name : 'RwLoadedProject',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedPackages',
		'isDirty'
	],
	#category : 'Rowan-Core'
}

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
	loadedPackages := KeyValueDictionary new
]

{ #category : 'initialization' }
RwLoadedProject >> initializeForLoadSpecification: aLoadSpecification [

	self initializeForName: aLoadSpecification specName.
	handle := aLoadSpecification
]

{ #category : 'testing' }
RwLoadedProject >> isDirty [

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
RwLoadedProject >> loadedPackagesDo: loadedPackageBlock loadedClassedDo: loadedClassBlock loadedClassExtenstionsDo: loadedClassExtensionBlock loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock [

	loadedPackages
		valuesDo: [ :loadedPackage | 
			loadedPackageBlock cull: self cull: loadedPackage.
			loadedPackage
				loadedClassedDo: loadedClassBlock
				loadedClassExtenstionsDo: loadedClassExtensionBlock
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

{ #category : 'definitions' }
RwLoadedProject >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: 'name' put: name.
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
