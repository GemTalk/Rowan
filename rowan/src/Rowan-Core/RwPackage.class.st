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

	^ self _packageTools query classesForPackageNamed: self name
]

{ #category : 'accessing' }
RwPackage >> extendedClasses [

	^ self _packageTools query classExtensionsForPackageNamed: self name
]

{ #category : 'testing' }
RwPackage >> isDirty [

	^self _loadedPackage isDirty

]
