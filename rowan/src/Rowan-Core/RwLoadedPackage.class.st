"
repository: The repository I was loaded from (if known, nil otherwise)
commitId: A repository-specific way of identifying source code commit, if it's a type of repository that has that kind of information (nil otherwise)
loadedClasses: Dictionary -- keys are (non-meta) classes, values are the LoadedClasses that I define.
loadedClassExtensions: -- Dictionary -- keys are (non-meta) classes, values are the LoadedClassExtensions that I define.
"
Class {
	#name : 'RwLoadedPackage',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'repository',
		'commitId',
		'loadedClasses',
		'loadedClassExtensions',
		'isDirty',
		'loadedProject'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwLoadedPackage >> addLoadedClass: aLoadedClass [

	self markDirty.
	loadedClasses at: aLoadedClass key put: aLoadedClass.
	aLoadedClass loadedPackage: self
]

{ #category : 'accessing' }
RwLoadedPackage >> addLoadedClassExtension: aLoadedClassExtension [

	self markDirty.
	loadedClassExtensions at: aLoadedClassExtension key
		put: aLoadedClassExtension.
	aLoadedClassExtension loadedPackage: self
]

{ #category : 'definitions' }
RwLoadedPackage >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	^RwPackageDefinition
		withProperties: self propertiesForDefinition
		classDefinitions: self classDefinitions
		classExtensions: self classExtensionDefinitions
]

{ #category : 'private' }
RwLoadedPackage >> classDefinitions [
	"Create definitions from all of the classes I define, and answer the collection of them"

	^self definitionsFor: loadedClasses
]

{ #category : 'private' }
RwLoadedPackage >> classExtensionDefinitions [
	"Create definitions from all of the class extensions I define, and answer the collection of them"

	^self definitionsFor: loadedClassExtensions
]

{ #category : 'queries' }
RwLoadedPackage >> classOrExtensionForClassNamed: className ifAbsent: absentBlock [

	^loadedClasses at: className
		ifAbsent: [loadedClassExtensions at: className ifAbsent: absentBlock]
]

{ #category : 'testing' }
RwLoadedPackage >> isDirty [

	^ isDirty
]

{ #category : 'testing' }
RwLoadedPackage >> isEmpty [

	^loadedClasses isEmpty and: [loadedClassExtensions isEmpty]
]

{ #category : 'accessing' }
RwLoadedPackage >> key [

	^ name
]

{ #category : 'enumeration' }
RwLoadedPackage >> loadedClassedDo: loadedClassBlock loadedClassExtenstionsDo: loadedClassExtensionBlock loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock [

	loadedClasses
		valuesDo: [ :loadedClass | 
			loadedClassBlock cull: self loadedProject cull: self cull: loadedClass.
			loadedClass
				loadedInstanceMethodsDo: loadedInstanceMethodBlock
				loadedClassMethodsDo: loadedClassMethodBlock ].
	loadedClassExtensions
		valuesDo: [ :loadedClassExtension | 
			loadedClassExtensionBlock
				cull: self loadedProject
				cull: self
				cull: loadedClassExtension.
			loadedClassExtension
				loadedInstanceMethodsDo: loadedInstanceMethodBlock
				loadedClassMethodsDo: loadedClassMethodBlock ]
]

{ #category : 'accessing' }
RwLoadedPackage >> loadedClasses [
	^loadedClasses

]

{ #category : 'accessing' }
RwLoadedPackage >> loadedClassExtensions [
	^loadedClassExtensions

]

{ #category : 'accessing' }
RwLoadedPackage >> loadedProject [

   ^loadedProject

]

{ #category : 'accessing' }
RwLoadedPackage >> loadedProject: anObject [

   loadedProject := anObject

]

{ #category : 'private' }
RwLoadedPackage >> markDirty [
	"I have been changed independent of a change to my repository, and therefore may be out of sync with my repository"

	isDirty := true
]

{ #category : 'private' }
RwLoadedPackage >> markNotDirty [
	"I have been changed to be consistent with my source, so mark not dirty."

	isDirty := false
]

{ #category : 'private' }
RwLoadedPackage >> markPackageDirty [

	self markDirty
]

{ #category : 'accessing' }
RwLoadedPackage >> projectName [

	^ loadedProject name
]

{ #category : 'accessing' }
RwLoadedPackage >> removeLoadedClass: aLoadedClass [

	self markDirty.
	loadedClasses removeKey: aLoadedClass key.
	aLoadedClass loadedPackage: nil
]

{ #category : 'accessing' }
RwLoadedPackage >> removeLoadedClassExtension: aLoadedClassExtension [

	self markDirty.
	loadedClassExtensions removeKey: aLoadedClassExtension key.
	aLoadedClassExtension loadedPackage: nil
]
