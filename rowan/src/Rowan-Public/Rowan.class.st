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
Rowan class >> projectNames [

	"Return list of project names"

	^ self image projectNames
]

{ #category : 'public tools' }
Rowan class >> projectTools [

	^self platform projectTools
]

{ #category : 'public' }
Rowan class >> unpackagedName [

	"Answer the name used for unpackaged projects and packages"

	^ '(NONE)'
]
