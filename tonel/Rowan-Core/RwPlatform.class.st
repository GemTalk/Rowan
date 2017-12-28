"
Provides services specific to the Smalltalk dialect being run,
and locates other services that are dialect-specific.
"
Class {
	#name : 'RwPlatform',
	#superclass : 'Object',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwPlatform class >> current [
	"Figure out what platform we're running on, then create and answer an instance of my appropriate subclass."

	| platformSymbol |
	platformSymbol := self currentPlatformSymbol.
	^platformSymbol == #GS ifTrue: [self forGemStone]
]

{ #category : 'private' }
RwPlatform class >> currentPlatformSymbol [
	"Answer a Symbol indicating which flavor of Smalltalk we're running in."

	(Object class canUnderstand: #_topazFileoutDefn) ifTrue: [^#GS].
	self error: 'We appear to be running on an unsupported platform'
]

{ #category : 'private' }
RwPlatform class >> forGemStone [

	| platformClass |
	platformClass := RwPlatformSpecificNameResolver
				resolve: 'RwGsPlatform'
				ifAbsent: 
					[self
						error: 'The CypressGemStone prerequisite package does not seem to be installed.'].
	^platformClass new
]

{ #category : 'unknown' }
RwPlatform >> fileUtilities [
	"Answer the platform-specific object for accessing files and directories"

	self subclassResponsibility
]

{ #category : 'queries' }
RwPlatform >> loadedPackageClass [
	"Answer the platform-specific class for LoadedPackage"

	self subclassResponsibility
]
