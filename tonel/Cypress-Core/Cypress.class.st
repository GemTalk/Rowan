"
No class-specific documentation for Cypress, hierarchy is: 
Object
  Cypress

"
Class {
	#name : 'Cypress',
	#superclass : 'Object',
	#classInstVars : [
		'knownRepositories',
		'configuration',
		'platform'
	],
	#category : 'Cypress-Core'
}

{ #category : 'public' }
Cypress class >> addRepositoryNamed: urlString [
	"Register a repository.
	Note that duplicate registrations are ignored, since knownRepositories is a Set"

	knownRepositories
		add: (CypRepository onUrl: (CypUrl fromString: urlString))
]

{ #category : 'public' }
Cypress class >> configuration [

	^configuration
]

{ #category : 'public' }
Cypress class >> diffPackages: beforePackages with: afterPackages [
	"Compare two PackageSets, answering a PackageSetModification"

	| beforeDefinition afterDefinition |
	beforeDefinition := beforePackages asDefinition.
	afterDefinition := afterPackages asDefinition.
	^afterDefinition compareAgainstBase: beforeDefinition
]

{ #category : 'private' }
Cypress class >> existingOrNewLoadedPackageNamed: aName [

	^platform loadedPackageClass existingOrNewPackageNamed: aName
]

{ #category : 'private' }
Cypress class >> fileUtilities [
	"Private to the Cypress system."

	^platform fileUtilities
]

{ #category : 'private' }
Cypress class >> image [
	"Private to the Cypress system."

	^platform image
]

{ #category : 'private' }
Cypress class >> initialize [
	"Cypress initialize."

	knownRepositories := Set new.
	platform := CypPlatform current.
	configuration := platform defaultConfiguration.
	platform initialize
]

{ #category : 'public' }
Cypress class >> knownRepositories [

	^knownRepositories
]

{ #category : 'private' }
Cypress class >> loadedPackageNamed: aName ifAbsent: absentBlock [

	^platform loadedPackageClass existingPackageNamed: aName
		ifAbsent: absentBlock
]

{ #category : 'private' }
Cypress class >> loadedPackageOfClass: aClass [
	"If the given class is in a package, answer that LoadedPackage. If not, answer nil."

	^platform loadedPackageOfClass: aClass
]

{ #category : 'public' }
Cypress class >> loadedPackagesNamed: someNames [
	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	| loadedPackages |
	self updateLoadedPackages.
	loadedPackages := someNames
				collect: [:aName | self loadedPackageNamed: aName ifAbsent: [nil]].
	^CypPackageSet withAll: (loadedPackages select: [:each | each notNil])
]

{ #category : 'public' }
Cypress class >> packageNamed: aName [
	"Answer a PackageReference, if the given package name is unique in all registered repositories. If the name resolves 0 or >=2 times, error."

	| token resolutions |
	token := 'notFound'.
	resolutions := (self knownRepositories
				collect: [:repo | repo packageNamed: aName ifAbsent: [token]])
					select: [:each | each ~~ token].
	resolutions isEmpty ifTrue: [self error: 'Named package not found'].
	resolutions size > 1
		ifTrue: [self error: 'Package name is not unique in registered repositories.'].
	^resolutions any
]

{ #category : 'public' }
Cypress class >> packagesNamed: someNames [
	"Create a PackageSet by names only, resolved from known repositories"

	| packageRefs |
	packageRefs := someNames collect: [:aName | self packageNamed: aName].
	^CypPackageSet withAll: packageRefs
]

{ #category : 'public' }
Cypress class >> platform [

	^platform
]

{ #category : 'public' }
Cypress class >> repositoryNamed: urlString [
	"Answer a Repository"

	^CypRepository onUrl: (CypUrl fromString: urlString)
]

{ #category : 'private' }
Cypress class >> updateLoadedPackages [
	"Update all LoadedPackages and their contents, based on the configured packageInfoSource."

	| source |
	source := configuration packageInfoSource.
	"If the source is Cypress, no updating is needed -- the LoadedPackages *are* the authoritative source"
	source == #Cypress ifFalse: [platform updateLoadedPackagesFrom: source]
]
