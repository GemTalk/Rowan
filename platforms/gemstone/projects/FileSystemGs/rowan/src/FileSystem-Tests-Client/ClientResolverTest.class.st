Class {
	#name : 'ClientResolverTest',
	#superclass : 'FileSystemResolverTest',
	#category : 'FileSystem-Tests-Client'
}

{ #category : 'running' }
ClientResolverTest >> createResolver [
	^ ClientResolver forCurrentPlatform
]

{ #category : 'tests' }
ClientResolverTest >> testClientHome [
	| reference |
	resolver := ClientResolver forCurrentPlatform.
	reference := resolver resolve: #home.
	self assert: (reference isKindOf: FileReference).
"	self assert: reference exists."	"Currently cannot lookup directory on client"
	self assert: reference isAbsolute.
	"self assert: reference isDirectory"  "Currently cannot lookup directory on client"
]
