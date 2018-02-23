Class {
	#name : 'RwPackageToolTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tode'
}

{ #category : 'tests - packages' }
RwPackageToolTest >> testAddPackage [

	| packageTools |
	packageTools := Rowan packageTools.
	packageTools create createPackagesNamed: #('Simple-Package')
]

{ #category : 'tests - packages' }
RwPackageToolTest >> testMovePackage [

	
]

{ #category : 'tests - packages' }
RwPackageToolTest >> testMovePackageToProject [

	
]

{ #category : 'tests - packages' }
RwPackageToolTest >> testRemovePackage [

	
]
