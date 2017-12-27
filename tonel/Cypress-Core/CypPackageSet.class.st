"
Holds some number of PackageReferences or LoadedPackages to be operated on together.
"
Class {
	#name : 'CypPackageSet',
	#superclass : 'Object',
	#instVars : [
		'packages'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypPackageSet class >> withAll: somePackageRefs [

	^(self new)
		initialize;
		addAll: somePackageRefs;
		yourself
]

{ #category : 'accessing' }
CypPackageSet >> addAll: somePackageRefs [

	somePackageRefs
		do: [:packageRef | packages at: packageRef name put: packageRef].
	^somePackageRefs
]

{ #category : 'definitions' }
CypPackageSet >> asDefinition [
	"Read the full definitions of each of my PackageReferences from its repository, and answer the resulting PackageSetDefinition. "

	| result |
	result := CypPackageSetDefinition new.
	packages do: [:packageRef | result addPackage: packageRef asDefinition].
	^result
]

{ #category : 'initialization' }
CypPackageSet >> initialize [

	packages := Dictionary new
]
