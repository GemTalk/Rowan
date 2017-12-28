"
Holds some number of PackageReferences or LoadedPackages to be operated on together.
"
Class {
	#name : 'RwPackageSet',
	#superclass : 'Object',
	#instVars : [
		'packages'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwPackageSet class >> withAll: somePackageRefs [

	^(self new)
		initialize;
		addAll: somePackageRefs;
		yourself
]

{ #category : 'accessing' }
RwPackageSet >> addAll: somePackageRefs [

	somePackageRefs
		do: [:packageRef | packages at: packageRef name put: packageRef].
	^somePackageRefs
]

{ #category : 'definitions' }
RwPackageSet >> asDefinition [
	"Read the full definitions of each of my PackageReferences from its repository, and answer the resulting PackageSetDefinition. "

	| result |
	result := RwPackageSetDefinition new.
	packages do: [:packageRef | result addPackage: packageRef asDefinition].
	^result
]

{ #category : 'initialization' }
RwPackageSet >> initialize [

	packages := Dictionary new
]
