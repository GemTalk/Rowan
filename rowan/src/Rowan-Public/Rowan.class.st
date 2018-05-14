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
	#category : 'Rowan-Public'
}

{ #category : 'public' }
Rowan class >> configuration [

	^configuration
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
Rowan class >> loadedPackageNamed: aName ifAbsent: absentBlock [

	^ self image loadedPackageNamed: aName ifAbsent: absentBlock
]

{ #category : 'private' }
Rowan class >> loadedPackageOfClass: aClass [
	"If the given class is in a package, answer that LoadedPackage. If not, answer nil."

	^self platform loadedPackageOfClass: aClass
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
	^ (self globalNamed: 'RwEntitySet') withAll: (loadedProjects select: [ :each | each notNil ])
]

{ #category : 'public' }
Rowan class >> packageNames [

	"Return list of package names"

	^ self image packageNames
]

{ #category : 'public tools' }
Rowan class >> packageTools [

	^self platform packageTools
]

{ #category : 'public' }
Rowan class >> projectNamed: aString [

	"Look up a project"

	^ self
		loadedProjectNamed: aString
		ifAbsent: [ self error: 'Project ' , aString printString , ' not found' ]

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
Rowan class >> repositoryNamed: urlString [
	"Answer a Repository"

	^(self globalNamed: 'CypRepository') onUrl: ((self globalNamed: 'CypUrl') fromString: urlString)
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
