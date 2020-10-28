"
Provides services specific to the Smalltalk dialect being run,
and locates other services that are dialect-specific.
"
Class {
	#name : 'RwPlatform',
	#superclass : 'Object',
	#category : 'Rowan-Kernel'
}

{ #category : 'automatic class initialization' }
RwPlatform >> _automaticClassInitializationBlackList_symbol [

	^#automaticClassInitializationBlackList
]

{ #category : 'automatic class initialization' }
RwPlatform >> automaticClassInitializationBlackList [

	"Answer list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	^ self 
		preferenceFor: preferenceSymbol 
		ifAbsent: [
			| list |
			list := OrderedCollection new.
			self setPreferenceFor: preferenceSymbol to: list.
			list]
]

{ #category : 'automatic class initialization' }
RwPlatform >> automaticClassInitializationBlackList_default [

	"Answer default list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	^ self 
		defaultPreferenceFor: preferenceSymbol 
		ifAbsent: [
			| list |
			list := OrderedCollection new.
			self setDefaultPreferenceFor: preferenceSymbol to: list.
			list]
]

{ #category : 'automatic class initialization' }
RwPlatform >> automaticClassInitializationBlackList_global [

	"Answer global list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	^ self 
		globalPreferenceFor: preferenceSymbol 
		ifAbsent: [
			| list |
			list := OrderedCollection new.
			self setDefaultPreferenceFor: preferenceSymbol to: list.
			list]
]

{ #category : 'queries' }
RwPlatform >> basePlatformConditionalAttribute [
	"Answer the generic conditional attribute for the platform"

	"Examples include: 'gemstone', 'pharo', 'squeak', 'vast' 
		(see RwSpecification class >> _supportedPlatformNames)"

	self subclassResponsibility: #'basePlatformConditionalAttribute'
]

{ #category : 'preferences' }
RwPlatform >> clearAllPreferencesFor: preferenceSymbol [ 

	self subclassResponsibility: #clearAllPreferencesFor:
]

{ #category : 'automatic class initialization' }
RwPlatform >> clearAutomaticClassInitializationBlackList [

	"Answer list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	self clearPreferenceFor: preferenceSymbol
]

{ #category : 'automatic class initialization' }
RwPlatform >> clearAutomaticClassInitializationBlackList_default [

	"Answer default list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	self clearDefaultPreferenceFor: preferenceSymbol
]

{ #category : 'automatic class initialization' }
RwPlatform >> clearAutomaticClassInitializationBlackList_global [

	"Answer default list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	self clearGlobalPreferenceFor: preferenceSymbol
]

{ #category : 'preferences' }
RwPlatform >> clearDefaultPreferenceFor: preferenceSymbol [ 

	self subclassResponsibility: #clearDefaultPreferenceFor:
]

{ #category : 'preferences' }
RwPlatform >> clearPreferenceFor: preferenceSymbol [ 

	self subclassResponsibility: #clearPreferenceFor:
]

{ #category : 'preferences' }
RwPlatform >> defaultPreferenceFor: preferenceSymbol [

	^ self defaultPreferenceFor: preferenceSymbol ifAbsent: [ self error: 'No preference found for ', preferenceSymbol asString printString ]
]

{ #category : 'preferences' }
RwPlatform >> defaultPreferenceFor: preferenceSymbol ifAbsent: aBlock [

	^ self subclassResponsibility: #defaultPreferenceFor:ifAbsent:
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
RwPlatform >> platformConditionalAttributes [
	"Answer the platform conditional attributes. Currently 'common' is the only shared attribute."

	"subclasses need to override and provide additional platform-specific attributes"

	^ #('common')
]

{ #category : 'preferences' }
RwPlatform >> preferenceFor: preferenceSymbol [

	^ self preferenceFor: preferenceSymbol ifAbsent: [ self error: 'No preference found for ', preferenceSymbol asString printString ]
]

{ #category : 'preferences' }
RwPlatform >> preferenceFor: preferenceSymbol ifAbsent: aBlock [

	^ self subclassResponsibility: #preferenceFor:ifAbsent:
]

{ #category : 'preferences' }
RwPlatform >> setDefaultPreferenceFor: preferenceSymbol to: anObject [

	self subclassResponsibility: #setDefaultPreferenceFor:to:
]

{ #category : 'preferences' }
RwPlatform >> setPreferenceFor: preferenceSymbol to: anObject [

	self subclassResponsibility: #setPreferenceFor:to:
]

{ #category : 'queries' }
RwPlatform >> tools [
	"Answer the platform-specific class for tools"

	self subclassResponsibility: #tools

]
