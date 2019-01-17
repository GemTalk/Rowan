"
SUnit tests for PlatformResolver
"
Class {
	#name : 'PlatformResolverTest',
	#superclass : 'FileSystemResolverTest',
	#category : 'FileSystem-Tests-Core-Resolver'
}

{ #category : 'running' }
PlatformResolverTest >> createResolver [
	^ PlatformResolver forCurrentPlatform
]

{ #category : 'tests' }
PlatformResolverTest >> testCache [
	| cache |
	cache := self assertOriginResolves: #cache
]

{ #category : 'tests' }
PlatformResolverTest >> testClientHome [
	| reference |
	resolver := ClientResolver forCurrentPlatform.
	reference := resolver resolve: #home.
	self assert: (reference isKindOf: FileReference).
"	self assert: reference exists."	"Currently cannot lookup directory on client"
	self assert: reference isAbsolute.
	"self assert: reference isDirectory"  "Currently cannot lookup directory on client"
]

{ #category : 'tests' }
PlatformResolverTest >> testHome [
	| home |
	home := self assertOriginResolves: #home.
	self assert: home isDirectory
]
