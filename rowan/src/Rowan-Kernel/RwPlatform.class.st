"
Provides services specific to the Smalltalk dialect being run,
and locates other services that are dialect-specific.
"
Class {
	#name : 'RwPlatform',
	#superclass : 'Object',
	#category : 'Rowan-Kernel'
}

{ #category : 'queries' }
RwPlatform >> basePlatformAttribute [
	"Answer the generic configuration attribute for the platform"

	"Examples include: 'gemstone', 'pharo', 'squeak'"


	self subclassResponsibility: #basePlatformAttribute

]

{ #category : 'queries' }
RwPlatform >> fileUtilities [
	"Answer the platform-specific object for accessing files and directories"

	self subclassResponsibility: #fileUtilities

]

{ #category : 'queries' }
RwPlatform >> globalNamed: aString [

	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	self subclassResponsibility: #'globalNamed:'
]

{ #category : 'queries' }
RwPlatform >> loadedPackageClass [
	"Answer the platform-specific class for LoadedPackage"

	self subclassResponsibility: #'loadedPackageClass'

]

{ #category : 'queries' }
RwPlatform >> platformConfigurationAttributes [
	"Answer the configuration attributes. Currently 'common' is the only shared attribute."

	"subclasses need to override and provide additional platform-specific attributes"

	^ #('common')

]

{ #category : 'queries' }
RwPlatform >> tools [
	"Answer the platform-specific class for tools"

	self subclassResponsibility: #tools

]
