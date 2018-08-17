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
Rowan class >> classServiceClass [

	^ self platform classServiceClass

]

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

{ #category : 'public client services' }
Rowan class >> jadeServerClassNamed: className [

	| jadeClasses |
	jadeClasses := Array with: JadeServer. 
	jadeClasses add: JadeServer64bit32. 
	^jadeClasses detect:[:cls | cls name == className] ifNone:[self error: 'Could not look up a JadeServer class: ', className]
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
Rowan class >> platformConfigurationAttributes [

	"Return list of platform-specific configuration attributes"

	^ self platform platformConfigurationAttributes

]

{ #category : 'public' }
Rowan class >> projectNames [

	"Return list of project names"

	^ self image projectNames
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

	"Answer the name used for unpackaged projects and packages"

	^ '(NONE)'
]
