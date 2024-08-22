Class {
	#name : 'RwPackage',
	#superclass : 'RowanInterface',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwPackage >> _loadedPackage [

	^ Rowan image loadedPackageNamed: self name
]

{ #category : 'private' }
RwPackage >> _loadedProject [

	^ self _loadedPackage loadedProject
]

{ #category : 'accessing' }
RwPackage >> definedClasses [

"	^ self _packageTools query classesForPackageNamed: self name"

	^ self error: 'not yet implemented'
]

{ #category : 'accessing' }
RwPackage >> definedClassNames [
	^ self _loadedPackage loadedClasses keys asSet
]

{ #category : 'accessing' }
RwPackage >> definedTraitNames [
	^ self _loadedPackage loadedTraits keys asSet
]

{ #category : 'accessing' }
RwPackage >> definedTraits [
	^ self error: 'not yet implemented'
]

{ #category : 'accessing' }
RwPackage >> extendedClasses [

"	^ self _packageTools query classExtensionsForPackageNamed: self name"

	^ self error: 'not yet implemented'
]

{ #category : 'accessing' }
RwPackage >> extendedClassNames [
	^ self _loadedPackage loadedClassExtensions keys asSet
]

{ #category : 'testing' }
RwPackage >> isDirty [
	"a project is dirty if it has changes that are not written to disk, or it's packages 
		have changes that are not written to  disk."

	^ self _loadedPackage isDirty
]

{ #category : 'testing' }
RwPackage >> isEmpty [

	^ self _loadedPackage isEmpty
]

{ #category : 'accessing' }
RwPackage >> project [
	^ RwProject newNamed: self _loadedProject name
]
