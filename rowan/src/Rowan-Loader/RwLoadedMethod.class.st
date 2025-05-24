"
No class-specific documentation for CypLoadedMethod, hierarchy is: 
Object
  CypLoadedThing( properties name handle)
    CypLoadedMethod

"
Class {
	#name : 'RwLoadedMethod',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedClass',
		'classIsMeta',
		'isDirty'
	],
	#category : 'Rowan-Loader'
}

{ #category : 'definitions' }
RwLoadedMethod >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	^RwMethodDefinition withProperties: self propertiesForDefinition
		source: self source
]

{ #category : 'accessing' }
RwLoadedMethod >> classIsMeta [

	^classIsMeta
]

{ #category : 'testing' }
RwLoadedMethod >> isDirty [
	"a method is dirty if it has changes that are not written to disk"

	^  isDirty
]

{ #category : 'comparing' }
RwLoadedMethod >> isEmpty [

	^super isEmpty & self source isNil
]

{ #category : 'testing' }
RwLoadedMethod >> isRowanLoadedMethod [
	"Answer true if the receiver is a loaded method ... used for determining the _owner of compiled method"

	^true
]

{ #category : 'queries' }
RwLoadedMethod >> loadedClass [

	^loadedClass
]

{ #category : 'accessing' }
RwLoadedMethod >> loadedClass: aLoadedClass [

	loadedClass := aLoadedClass
]

{ #category : 'queries' }
RwLoadedMethod >> loadedPackage [

	^self loadedClass loadedPackage
]

{ #category : 'private' }
RwLoadedMethod >> markDirty [
	"I have been changed independent of a change to my repository, and therefore may be out of sync with my repository"

	isDirty := true.
	self markPackageDirty
]

{ #category : 'private' }
RwLoadedMethod >> markNotDirty [
	"I have been changed to be consistent with my source, so mark not dirty."

	isDirty ifTrue: [ isDirty := false]
]

{ #category : 'private' }
RwLoadedMethod >> markPackageDirty [
	"LoadedClass may still be nil if we're creating a new LoadedMethod."

	loadedClass ifNotNil: [loadedClass markPackageDirty]
]

{ #category : 'accessing' }
RwLoadedMethod >> methodCategory [

	^ properties at: #'protocol'

]

{ #category : 'definitions' }
RwLoadedMethod >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: #'selector' put: name.
	^props
]

{ #category : 'accessing' }
RwLoadedMethod >> selector [

	^ properties at: #'selector'
]

{ #category : 'accessing' }
RwLoadedMethod >> source [

	self subclassResponsibility
]
