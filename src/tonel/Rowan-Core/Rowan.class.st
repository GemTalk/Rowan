"
No class-specific documentation for Cypress, hierarchy is: 
Object
  Cypress

"
Class {
	#name : 'Rowan',
	#superclass : 'Object',
	#classInstVars : [
		'configuration',
		'platform'
	],
	#category : 'Rowan-Core'
}

{ #category : 'public' }
Rowan class >> addProject: aRwProjectSpecification [

	"Register a project"

	^ self image addProject: aRwProjectSpecification
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

{ #category : 'public' }
Rowan class >> globalNamed: aName [

	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ self platform globalNamed: aName
]

{ #category : 'private' }
Rowan class >> image [
	"Private to the Cypress system."

	^platform image
]

{ #category : 'private' }
Rowan class >> initialize [

	"Rowan initialize."

	platform := RwPlatform current.
	configuration := platform defaultConfiguration.
	platform initialize
]

{ #category : 'public' }
Rowan class >> loadedPackageDefinitionsNamed: someNames [

	"Answer a PackageSetDefinition containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	^ (self loadedPackagesNamed: someNames) asPackageDefinitionSet
]

{ #category : 'public' }
Rowan class >> loadedPackageDefinitionsNamed: someNames forProject: projectDefinition [

	"Answer a PackageSetDefinition containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	^ (self loadedPackagesNamed: someNames forProject: projectDefinition)
		asPackageDefinitionSet
]

{ #category : 'private' }
Rowan class >> loadedPackageNamed: aName ifAbsent: absentBlock [

	^ self image loadedPackageNamed: aName ifAbsent: absentBlock
]

{ #category : 'private' }
Rowan class >> loadedPackageOfClass: aClass [
	"If the given class is in a package, answer that LoadedPackage. If not, answer nil."

	^platform loadedPackageOfClass: aClass
]

{ #category : 'public' }
Rowan class >> loadedPackagesForProject: projectDefinition [

	"Answer a PackageSet containing packages for the given project definition."

	^ self
		loadedPackagesNamed: projectDefinition loadedPackageNames
		forProject: projectDefinition
]

{ #category : 'public' }
Rowan class >> loadedPackagesNamed: someNames [

	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	"very likely we should be dealing with a project name 
at this point ... unless there is a use case for loading a collection of packages without directly using the project interface --- loaded packages do know what project they belong to ... so there might be something done behind the scenes"

	| loadedPackages |
	self updateLoadedPackages.
	loadedPackages := someNames
		collect: [ :aName | self loadedPackageNamed: aName ifAbsent: [ nil ] ].
	^ RwEntitySet withAll: (loadedPackages select: [ :each | each notNil ])
]

{ #category : 'public' }
Rowan class >> loadedPackagesNamed: someNames forProject: projectDefinition [

	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	"very likely we should be dealing with a project name 
at this point ... unless there is a use case for loading a collection of packages without directly using the project interface --- loaded packages do know what project they belong to ... so there might be something done behind the scenes"

	| loadedPackages |
	self updateLoadedPackages.
	loadedPackages := someNames
		collect: [ :aName | self loadedPackageNamed: aName ifAbsent: [ nil ] ].
	^ RwEntitySet
		withAll:
			(loadedPackages
				select: [ :loadedPackage | 
					loadedPackage notNil
						and: [ loadedPackage loadedProject name = projectDefinition name ] ])
]

{ #category : 'private' }
Rowan class >> loadedProjectNamed: aName ifAbsent: absentBlock [

	^ self image loadedProjectNamed: aName ifAbsent: absentBlock
]

{ #category : 'public' }
Rowan class >> loadedProjectsNamed: someNames [

	"Answer a RwProjectSetDefinition containing projects with the given names in the image.
 	If any name is not a loaded project, that name is ignored."

	| loadedProjects |
	loadedProjects := someNames
		collect: [ :aName | self loadedProjectNamed: aName ifAbsent: [ nil ] ].
	^ RwEntitySet withAll: (loadedProjects select: [ :each | each notNil ])
]

{ #category : 'public tools' }
Rowan class >> packageTools [

	^self platform packageTools
]

{ #category : 'public' }
Rowan class >> platform [

	^ platform ifNil: [ platform := RwPlatform current ]
]

{ #category : 'public' }
Rowan class >> projectNamed: aString [

	"Look up a project"

	^ self
		projectNamed: aString
		ifAbsent: [ self error: 'Project ' , aString printString , ' not found' ]
]

{ #category : 'public' }
Rowan class >> projectNamed: aString ifAbsent: absentBlock [

	"Look up a project"

	^ self image projectNamed: aString ifAbsent: absentBlock
]

{ #category : 'public' }
Rowan class >> projectNames [

	"Return list of project names"

	^ self image projectNames
]

{ #category : 'public tools' }
Rowan class >> projectTools [

	^self platform projectTools
]

{ #category : 'public' }
Rowan class >> removeProject: aRwProjectSpecification [

	"remove a project"

	self image removeProject: aRwProjectSpecification
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

	"If the source is Cypress or SymbolDictionary, no updating is needed -- 
		the LoadedPackages *are* the authoritative source -- Category source requires updating"

	| source |
	source := configuration packageInfoSource.
	source == #'Category'
		ifTrue: [ platform updateLoadedPackagesFrom: source ]
]
