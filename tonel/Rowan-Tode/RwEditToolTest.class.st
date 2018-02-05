Class {
	#name : 'RwEditToolTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tode'
}

{ #category : 'tests - classes' }
RwEditToolTest >> testAddClass [

	
]

{ #category : 'tests - packages' }
RwEditToolTest >> testAddPackage [

	| packageTools |
	packageTools := Rowan packageTools.
	packageTools create createPackagesNamed: #('Simple-Package')
]

{ #category : 'tests - classes' }
RwEditToolTest >> testChangeClass [

	
]

{ #category : 'tests - classes' }
RwEditToolTest >> testRemoveClass [

	
]

{ #category : 'tests - packages' }
RwEditToolTest >> testRemovePackage [

	
]
