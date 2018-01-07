"
No class-specific documentation for Cypress, hierarchy is: 
Object
  Cypress

"
Class {
	#name : 'Rowan',
	#superclass : 'Object',
	#classInstVars : [
		'knownRepositories',
		'configuration',
		'platform',
		'knownProjects'
	],
	#category : 'Rowan-Core'
}

{ #category : 'public' }
Rowan class >> addProject: aRwProjectSpecification [
  "Register a project"

  knownProjects
    at: aRwProjectSpecification specName
    put: aRwProjectSpecification
]

{ #category : 'public' }
Rowan class >> addRepositoryNamed: urlString [
	"Register a repository.
	Note that duplicate registrations are ignored, since knownRepositories is a Set"

	knownRepositories
		add: (CypRepository onUrl: (CypUrl fromString: urlString))
]

{ #category : 'public' }
Rowan class >> configuration [

	^configuration
]

{ #category : 'public' }
Rowan class >> diffPackages: beforePackages with: afterPackages [
	"Compare two PackageSets, answering a PackageSetModification"

	| beforeDefinition afterDefinition |
	beforeDefinition := beforePackages asDefinition.
	afterDefinition := afterPackages asDefinition.
	^afterDefinition compareAgainstBase: beforeDefinition
]

{ #category : 'private' }
Rowan class >> existingOrNewLoadedPackageNamed: aName [

	^platform loadedPackageClass existingOrNewPackageNamed: aName
]

{ #category : 'private' }
Rowan class >> fileUtilities [
	"Private to the Cypress system."

	^platform fileUtilities
]

{ #category : 'private' }
Rowan class >> image [
	"Private to the Cypress system."

	^platform image
]

{ #category : 'private' }
Rowan class >> initialize [
  "Rowan initialize."

  knownRepositories := Set new.
  platform := RwPlatform current.
  configuration := platform defaultConfiguration.
  platform initialize.
  knownProjects := Dictionary new
]

{ #category : 'public' }
Rowan class >> knownProjects [
  ^ knownProjects
]

{ #category : 'public' }
Rowan class >> knownRepositories [

	^knownRepositories
]

{ #category : 'private' }
Rowan class >> loadedPackageNamed: aName ifAbsent: absentBlock [

	^platform loadedPackageClass existingPackageNamed: aName
		ifAbsent: absentBlock
]

{ #category : 'private' }
Rowan class >> loadedPackageOfClass: aClass [
	"If the given class is in a package, answer that LoadedPackage. If not, answer nil."

	^platform loadedPackageOfClass: aClass
]

{ #category : 'public' }
Rowan class >> loadedPackagesNamed: someNames [
	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	| loadedPackages |
	self updateLoadedPackages.
	loadedPackages := someNames
				collect: [:aName | self loadedPackageNamed: aName ifAbsent: [nil]].
	^RwPackageSet withAll: (loadedPackages select: [:each | each notNil])
]

{ #category : 'public' }
Rowan class >> packageNamed: aName [
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
Rowan class >> packagesNamed: someNames [
	"Create a PackageSet by names only, resolved from known repositories"

	| packageRefs |
	packageRefs := someNames collect: [:aName | self packageNamed: aName].
	^RwPackageSet withAll: packageRefs
]

{ #category : 'public tools' }
Rowan class >> packageTools [

	^self platform packageTools
]

{ #category : 'public' }
Rowan class >> platform [

	^platform
]

{ #category : 'public' }
Rowan class >> projectNamed: aString ifAbsent: absentBlock [
  "Look up a project"

  ^ knownProjects at: aString ifAbsent: absentBlock
]

{ #category : 'public tools' }
Rowan class >> projectTools [

	^self platform projectTools
]

{ #category : 'public' }
Rowan class >> repositoryNamed: urlString [
	"Answer a Repository"

	^CypRepository onUrl: (CypUrl fromString: urlString)
]

{ #category : 'public tools' }
Rowan class >> repositoryTools [

	^self platform repositoryTools
]

{ #category : 'private' }
Rowan class >> updateLoadedPackages [
	"Update all LoadedPackages and their contents, based on the configured packageInfoSource."

	| source |
	source := configuration packageInfoSource.
	"If the source is Cypress, no updating is needed -- the LoadedPackages *are* the authoritative source"
	source == #Cypress ifFalse: [platform updateLoadedPackagesFrom: source]
]
