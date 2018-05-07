"
Provides services specific to the Smalltalk dialect being run,
and locates other services that are dialect-specific.
"
Class {
	#name : 'RwPlatform',
	#superclass : 'Object',
	#category : 'Rowan-Public'
}

{ #category : 'unknown' }
RwPlatform >> fileUtilities [
	"Answer the platform-specific object for accessing files and directories"

	self subclassResponsibility
]

{ #category : 'queries' }
RwPlatform >> globalNamed: aString [

	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	self subclassResponsibility: #'globalNamed:'
]

{ #category : 'queries' }
RwPlatform >> loadedPackageClass [
	"Answer the platform-specific class for LoadedPackage"

	self subclassResponsibility
]

{ #category : 'queries' }
RwPlatform >> tools [
	"Answer the platform-specific class for tools"

	self subclassResponsibility
]
