"
I am an abstract superclass for platform-specific resolvers.
"
Class {
	#name : 'PlatformResolver',
	#superclass : 'FileSystemResolver',
	#category : 'FileSystem-Core-Base-Resolver'
}

{ #category : 'instance creation' }
PlatformResolver class >> forCurrentPlatform [
	^(self resolverClassFor: System osName) new
]

{ #category : 'accessing' }
PlatformResolver class >> platformNames [
	^ {}
]

{ #category : 'instance creation' }
PlatformResolver class >> resolverClassFor: anOsName [
	^PlatformResolver allSubclasses detect: [:ea | ea platformNames includes: anOsName]
]

{ #category : 'origins' }
PlatformResolver >> cache [
	"Operating Systems often define standard locations for a personal cache directory. The cache directory is a user-specific non-essential (cached) place where data should be written."
	self subclassResponsibility
]

{ #category : 'private' }
PlatformResolver >> cantFindOriginError [
	^ Error signal: 'Can''t find the requested origin' 
]

{ #category : 'origins' }
PlatformResolver >> desktop [
	^ self subclassResponsibility
]

{ #category : 'private' }
PlatformResolver >> directoryFromEnvVariableNamed: aString [
	^ self directoryFromEnvVariableNamed: aString or: [ self cantFindOriginError ]
]

{ #category : 'private' }
PlatformResolver >> directoryFromEnvVariableNamed: aString or: aBlock [
	| envValue |
	envValue := [ self osEnvironmentVariable: aString ]
		on: Error
		do: [ ^ aBlock value ].
	^ envValue isEmptyOrNil
		ifTrue: [ aBlock value ]
		ifFalse: [ self resolveString: envValue ].
]

{ #category : 'origins' }
PlatformResolver >> documents [
	^ self subclassResponsibility
]

{ #category : 'origins' }
PlatformResolver >> home [
	^ self directoryFromEnvVariableNamed: self homeEnvironmentVariableName
]

{ #category : 'origins' }
PlatformResolver >> homeEnvironmentVariableName [
	^ self subclassResponsibility
]

{ #category : 'private' }
PlatformResolver >> osEnvironmentVariable: aString [

	^System gemEnvironmentVariable: aString
]

{ #category : 'origins' }
PlatformResolver >> preferences [
	^ self subclassResponsibility
]

{ #category : 'resolving' }
PlatformResolver >> supportedOrigins [
	^ #(home desktop documents preferences cache temp)
]

{ #category : 'origins' }
PlatformResolver >> temp [
	"Where to put files that are not supposed to last long"
	^ self subclassResponsibility 
]
