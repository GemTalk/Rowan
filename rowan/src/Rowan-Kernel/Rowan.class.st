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
	#category : 'Rowan-Kernel'
}

{ #category : 'public client services' }
Rowan class >> answeringServiceClass [

	^ self platform answeringServiceClass
]

{ #category : 'public' }
Rowan class >> automaticClassInitializationBlackList [

	"Answer list of project names for which automatic class initialiation should be disabled."

	^ self platform automaticClassInitializationBlackList
]

{ #category : 'public client services' }
Rowan class >> browserServiceClass [

	^ self platform browserServiceClass
]

{ #category : 'public client services' }
Rowan class >> classServiceClass [

	^ self platform classServiceClass

]

{ #category : 'public tools' }
Rowan class >> classTools [

	^ self platform classTools
]

{ #category : 'public' }
Rowan class >> clearAutomaticClassInitializationBlackList [

	"Clear list of project names for which automatic class initialiation should be disabled."

	^ self platform clearAutomaticClassInitializationBlackList
]

{ #category : 'public' }
Rowan class >> clearDefaultAutomaticClassInitializationBlackList [

	"Clear default list of project names for which automatic class initialiation should be disabled.
		Individual users may override the black list."

	^ self platform clearAutomaticClassInitializationBlackList_default
]

{ #category : 'public client services' }
Rowan class >> commandResultClass [

	^ self platform commandResultClass
]

{ #category : 'public' }
Rowan class >> configuration [

	^configuration
]

{ #category : 'public' }
Rowan class >> defaultAutomaticClassInitializationBlackList [

	"Answer default list of project names for which automatic class initialiation should be disabled.
		Individual users may override the black list."

	^ self platform automaticClassInitializationBlackList_default
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

{ #category : 'public client services' }
Rowan class >> jadeServerClassNamed: className [

	^ self platform jadeServerClassNamed: className
]

{ #category : 'public client services' }
Rowan class >> loggingServiceClass [

	^ self platform loggingServiceClass
]

{ #category : 'public client services' }
Rowan class >> methodServiceClass [

	^ self platform methodServiceClass

]

{ #category : 'public' }
Rowan class >> packageNames [

	"Return list of package names"

	^ self image packageNames
]

{ #category : 'public client services' }
Rowan class >> packageServiceClass [

	^ self platform packageServiceClass

]

{ #category : 'public tools' }
Rowan class >> packageTools [

	^self platform packageTools
]

{ #category : 'private' }
Rowan class >> platform [

	^ RwPlatform current
]

{ #category : 'public' }
Rowan class >> platformConditionalAttributes [

	"Return list of platform-specific conditional attributes for use by components"

	^ self platform platformConditionalAttributes
]

{ #category : 'public' }
Rowan class >> projectNamed: aName [

	"Answer a project with the given name.  If no project with the given name is found, signals error."

	^ self 
		projectNamed: aName 
			ifPresent: [:loadedProject | loadedProject ]
			ifAbsent: [ self error: 'The project ', aName printString, ' was not found' ]
]

{ #category : 'public' }
Rowan class >> projectNamed: aName ifAbsent: absentBlock [

	"Lookup a project with the given name, if found return the project. if not found evaluate the <absentBlock>."

	^ self 
		projectNamed: aName 
			ifPresent: [:loadedProject | loadedProject ]
			ifAbsent: absentBlock
]

{ #category : 'public' }
Rowan class >> projectNamed: aName ifPresent: presentBlock [

	"Lookup a project with the given name, if found evaluate the <presentBlock>. if not found return nil."

	^ self 
		projectNamed: aName 
			ifPresent: presentBlock
			ifAbsent: []
]

{ #category : 'public' }
Rowan class >> projectNamed: aName ifPresent: presentBlock ifAbsent: absentBlock [

	"Lookup a project with the given name, if found evaluate the <presentBlock>, if not evaluate the <absentBlock.."

	^ self platform 
		projectNamed: aName 
			ifPresent: presentBlock 
			ifAbsent: absentBlock
]

{ #category : 'public' }
Rowan class >> projectNames [

	"Return list of project names"

	^ self image projectNames
]

{ #category : 'public' }
Rowan class >> projects [
	"Return list of projects"

	^ self projectNames collect: [ :projectName | self projectNamed: projectName ]
]

{ #category : 'public client services' }
Rowan class >> projectServiceClass [

	^ self platform projectServiceClass

]

{ #category : 'public tools' }
Rowan class >> projectTools [

	^self platform projectTools
]

{ #category : 'public client services' }
Rowan class >> serviceClass [

	^ self platform serviceClass

]

{ #category : 'public' }
Rowan class >> unpackagedName [
	"Answer the name used for projects and packages that are not in a package ... unpackaged projects and packages are where pacakge things go by default."

	^ '(NONE)'
]

{ #category : 'public' }
Rowan class >> unpackagedPackagePrefix [
	"Answer the prefix used for naming unpackaged packages"

	^ self unpackagedProjectName , '-'
]

{ #category : 'public' }
Rowan class >> unpackagedProjectName [
	"Answer the name of the unpackaged project that manages unpackaged things"

	^ 'UnPackaged'
]

{ #category : 'public' }
Rowan class >> version [
	^ RwSemanticVersionNumber fromString: self versionString
]

{ #category : 'public' }
Rowan class >> versionString [
	^ '2.4.1'
]
