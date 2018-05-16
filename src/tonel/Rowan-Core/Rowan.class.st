"
No class-specific documentation for Cypress, hierarchy is: 
Object
  Cypress

"
Class {
	#name : 'Rowan',
	#superclass : 'Object',
	#classInstVars : [
		'configuration'
	],
	#category : 'Rowan-Core'
}

{ #category : 'public' }
Rowan class >> addProject: aRwProjectSpecification [

	"Register a project"

	^ self image addProject: aRwProjectSpecification
]

{ #category : 'services temporary' }
Rowan class >> classServiceClass [

	^System myUserProfile symbolList objectNamed: #RowanClassService

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

	^self platform loadedPackageClass existingOrNewPackageNamed: aName
]

{ #category : 'private' }
Rowan class >> fileUtilities [
	"Private to the Cypress system."

	^self platform fileUtilities
]

{ #category : 'public tools' }
Rowan class >> gitTools [

	^ self platform gitTools
]

{ #category : 'public' }
Rowan class >> globalNamed: aName [

	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ self platform globalNamed: aName
]

{ #category : 'private' }
Rowan class >> image [
	"Private to the Cypress system."

	^self platform image
]

{ #category : 'private' }
Rowan class >> initialize [

	"Rowan initialize."

	configuration := self platform defaultConfiguration.
	self platform initialize
]

{ #category : 'private' }
Rowan class >> loadedHybridPackageNamed: aName ifAbsent: absentBlock [

	^ self image loadedHybridPackageNamed: aName ifAbsent: absentBlock
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

	^self platform loadedPackageOfClass: aClass
]

{ #category : 'public' }
Rowan class >> loadedPackagesForProject: projectDefinition [

	"Answer a PackageSet containing packages for the given project definition."

	^ self
		loadedPackagesNamed: projectDefinition packageNames
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

{ #category : 'services temporary' }
Rowan class >> methodServiceClass [

	^System myUserProfile symbolList objectNamed: #RowanMethodService

]

{ #category : 'public' }
Rowan class >> packageNames [

	"Return list of package names"

	^ self image packageNames
]

{ #category : 'services temporary' }
Rowan class >> packageServiceClass [

	^System myUserProfile symbolList objectNamed: #RowanPackageService

]

{ #category : 'public tools' }
Rowan class >> packageTools [

	^self platform packageTools
]

{ #category : 'public' }
Rowan class >> platform [

	^ RwPlatform current
]

{ #category : 'public' }
Rowan class >> projectNamed: aString [

	"Look up a project"

	^ self
		loadedProjectNamed: aString
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

{ #category : 'services temporary' }
Rowan class >> projectServiceClass [

	^System myUserProfile symbolList objectNamed: #RowanProjectService

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

{ #category : 'public' }
Rowan class >> unpackagedName [

	"Answer the name used for unpackaged projects and packages"

	^ '(NONE)'
]

{ #category : 'private' }
Rowan class >> updateLoadedPackages [

	"Update all LoadedPackages and their contents, based on the configured packageInfoSource."

	"If the source is Cypress or SymbolDictionary, no updating is needed -- 
		the LoadedPackages *are* the authoritative source -- Category source requires updating"

	| source |
	source := configuration packageInfoSource.
	source == #'Category'
		ifTrue: [ self platform updateLoadedPackagesFrom: source ]
]
