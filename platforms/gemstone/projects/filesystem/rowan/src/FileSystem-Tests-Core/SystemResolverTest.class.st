"
SUnit tests for SystemResolver
"
Class {
	#name : 'SystemResolverTest',
	#superclass : 'FileSystemResolverTest',
	#category : 'FileSystem-Tests-Core-Resolver'
}

{ #category : 'running' }
SystemResolverTest >> createResolver [
	^ SystemResolver new
]

{ #category : 'testing' }
SystemResolverTest >> testDbfScratchDir [
	self assertOriginResolves: #dbfScratchDir
]

{ #category : 'testing' }
SystemResolverTest >> testExtent1 [
	self assertOriginResolves: #extent1
]

{ #category : 'testing' }
SystemResolverTest >> testExtent1Directory [
	self assertOriginResolves: #extent1Directory
]

{ #category : 'testing' }
SystemResolverTest >> testImage [
	self assertOriginResolves: #image
]

{ #category : 'testing' }
SystemResolverTest >> testImageDirectory [
	self assertOriginResolves: #imageDirectory
]

{ #category : 'testing' }
SystemResolverTest >> testTranlog [
	self assertOriginResolves: #tranlog
]
