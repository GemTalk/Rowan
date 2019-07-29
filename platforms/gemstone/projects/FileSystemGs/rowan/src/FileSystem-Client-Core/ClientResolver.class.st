Class {
	#name : 'ClientResolver',
	#superclass : 'PlatformResolver',
	#instVars : [
		'resolver'
	],
	#category : 'FileSystem-Client-Core'
}

{ #category : 'instance creation' }
ClientResolver class >> forCurrentPlatform [
	^self new
		resolver: (self resolverClassFor: System clientOsName) new;
		yourself
]

{ #category : 'origins' }
ClientResolver >> cache [
	^ self resolver cache
]

{ #category : 'origins' }
ClientResolver >> desktop [
	^ self resolver desktop
]

{ #category : 'origins' }
ClientResolver >> documents [
	^ self resolver documents
]

{ #category : 'resolving' }
ClientResolver >> fileSystem [
	^ ClientStore currentFileSystem
]

{ #category : 'origins' }
ClientResolver >> homeEnvironmentVariableName [
	^ self resolver homeEnvironmentVariableName
]

{ #category : 'private' }
ClientResolver >> osEnvironmentVariable: aString [

	^System clientEnvironmentVariable: aString
]

{ #category : 'origins' }
ClientResolver >> preferences [
	^ self resolver preferences
]

{ #category : 'accessing' }
ClientResolver >> resolver [
	^resolver
]

{ #category : 'accessing' }
ClientResolver >> resolver: aPlatformResolver [
	resolver := aPlatformResolver
]

{ #category : 'resolving' }
ClientResolver >> supportedOrigins [
	^ self resolver supportedOrigins
]

{ #category : 'origins' }
ClientResolver >> temp [
	^ self resolver temp
]
