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

	^ self new initializeForLoadSpecification: aLoadSpecification
]

{ #category : 'accessing' }
RwLoadedProject >> addLoadedPackage: aLoadedPackage [

	self markDirty.
	loadedPackages at: aLoadedPackage key put: aLoadedPackage.
	aLoadedPackage loadedProject: self
]

{ #category : 'initialization' }
RwLoadedProject >> initialize [

	super initialize.
	loadedPackages := KeyValueDictionary new
]

{ #category : 'initialization' }
RwLoadedProject >> initializeForClass: aClass [

	self initialize.
	self initializeForName: aClass name asString.
	handle := aClass
]

{ #category : 'initialization' }
RwLoadedProject >> initializeForLoadSpecification: aLoadSpecification [

	self initialize.
	self initializeForName: aLoadSpecification specName.
	handle := aLoadSpecification
]

{ #category : 'testing' }
RwLoadedProject >> isEmpty [

	^ loadedPackages isEmpty
]

{ #category : 'accessing' }
RwLoadedProject >> loadedPackages [

   ^loadedPackages

]

{ #category : 'accessing' }
RwLoadedProject >> loadedPackages: anObject [

   loadedPackages := anObject

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
RwLoadedProject >> removeLoadedPackage: aLoadedPackage [

	self markDirty.
	loadedPackages removeKey: aLoadedPackage key
]
