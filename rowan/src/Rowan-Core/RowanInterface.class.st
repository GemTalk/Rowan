Class {
	#name : 'RowanInterface',
	#superclass : 'Object',
	#instVars : [
		'name'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RowanInterface class >> newNamed: aName [

	^ self new
		initializeForName: aName;
		yourself
]

{ #category : 'private' }
RowanInterface >> _loadedProject [

	^ self subclassResponsibility: #'_loadedProject'
]

{ #category : 'private' }
RowanInterface >> _packageTools [

	^ Rowan packageTools
]

{ #category : 'private' }
RowanInterface >> _projectTools [

	^ Rowan projectTools
]

{ #category : 'accessing' }
RowanInterface >> classes [

	| classes |
	classes := IdentitySet new.
	classes
		addAll: self extendedClasses;
		addAll: self definedClasses.
	^ classes
]

{ #category : 'accessing' }
RowanInterface >> definedClasses [

	^ self subclassResponsibility: #'definedClasses'
]

{ #category : 'accessing' }
RowanInterface >> extendedClasses [

	^ self subclassResponsibility: #'extendedClasses'
]

{ #category : 'initialization' }
RowanInterface >> initializeForName: aName [

	name := aName
]

{ #category : 'accessing' }
RowanInterface >> name [

	^ name
]

{ #category : 'accessing' }
RowanInterface >> project [

	^ RwProject newNamed: self _loadedProject name
]
