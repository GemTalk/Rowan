Class {
	#name : 'RwAbstractComponentDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'components',
		'packages'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwAbstractComponentDefinition class >> newNamed: componentName [

	^ self new
			name: componentName;
			yourself
]

{ #category : 'accessing' }
RwAbstractComponentDefinition >> addComponent: aComponentDefinition [

	| key |
	key := aComponentDefinition key.
	(components includesKey: key) ifTrue: [self error: 'Duplicate component'].
	^ components at: key put: aComponentDefinition
]

{ #category : 'accessing' }
RwAbstractComponentDefinition >> addComponentNamed: componentName [

	^ self addComponent: (RwComponentDefinition newNamed: componentName)
]

{ #category : 'accessing' }
RwAbstractComponentDefinition >> addPackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifTrue: [self error: 'Duplicate package'].
	^ packages at: key put: aPackageDefinition
]

{ #category : 'accessing' }
RwAbstractComponentDefinition >> addPackageNamed: packageName [

	^ self addPackage: (RwPackageDefinition newNamed: packageName)
]

{ #category : 'accessing' }
RwAbstractComponentDefinition >> addSubComponentNamed: subComponentName [

	^ self addComponent: (RwSubComponentDefinition newNamed: subComponentName)
]

{ #category : 'initialization' }
RwAbstractComponentDefinition >> initialize [

	super initialize.
	components := Dictionary new.
	packages := Dictionary new.
]

{ #category : 'accessing' }
RwAbstractComponentDefinition >> key [

	^properties at: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwAbstractComponentDefinition >> name [

	^ self key
]

{ #category : 'accessing' }
RwAbstractComponentDefinition >> name: aString [ 

	properties at: 'name' put: aString
]
