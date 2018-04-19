"
I'm a writer for tonel format
"
Class {
	#name : 'TonelCypressWriter',
	#superclass : 'CypressAbstractPackageWriter',
	#instVars : [
		'snapshot',
		'sourceDir',
		'packageDir',
		'writer'
	],
	#category : 'CypressTonel-Core'
}

{ #category : 'instance creation' }
TonelCypressWriter class >> on: sourceDir [
	^ self new
		sourceDir: sourceDir;
		yourself

]

{ #category : 'accessing' }
TonelCypressWriter >> definitions [

	^ snapshot definitions

]

{ #category : 'initializing - private' }
TonelCypressWriter >> initializeForRepository: aCypressFileSystemRepository [
  repository := aCypressFileSystemRepository.
  self sourceDir: repository directoryPath

]

{ #category : 'private testing' }
TonelCypressWriter >> isClass: aClassDefinition [
	^ aClassDefinition class = CypressClassDefinition

]

{ #category : 'private testing' }
TonelCypressWriter >> isTrait: aClassDefinition [
	^ false

]

{ #category : 'accessing' }
TonelCypressWriter >> packageDir [
	^ packageDir 
		ifNotNil: [ self fileUtils directoryFromPath: packageDir relativeTo: self sourceDir ]
		ifNil: [ self sourceDir  ]

]

{ #category : 'accessing' }
TonelCypressWriter >> sourceDir [
	^ sourceDir

]

{ #category : 'accessing' }
TonelCypressWriter >> sourceDir: aFileReference [
	sourceDir := aFileReference

]

{ #category : 'private writing' }
TonelCypressWriter >> writeMethodExtensions [
	| classesAndMethods |
	classesAndMethods := Dictionary new.
	snapshot definitions 
		select: [ :each | each isMethodDefinition and: [ each isExtensionMethod ] ]
		thenDo: [ :each | 
			(classesAndMethods 
				at: each className
				ifAbsentPut: [ Set new ])
				add: each ].
	classesAndMethods keysAndValuesDo: [ :className :methods | 
		self writer
			writeExtensionMethods: methods 
			className: className ]
		
	

]

{ #category : 'private writing' }
TonelCypressWriter >> writePackage: aPackageName [
	"I'm assuming first category is actually the package"
	packageDir := aPackageName.
	(self fileUtils directoryExists: self packageDir) ifTrue: [ 
		self fileUtils deleteAll: self packageDir ].  
	self fileUtils ensureDirectoryExists: self packageDir.
	self writer writePackage: packageDir

]

{ #category : 'private writing' }
TonelCypressWriter >> writePackageFileNamed: packageFileName do: writerBlock [

	self fileUtils 
		writeStreamFor: packageFileName
		in: self packageDir 
		do: [ :aStream | writerBlock value: aStream ]

]

{ #category : 'accessing' }
TonelCypressWriter >> writer [
  ^ writer
    ifNil: [ 
      writer := TonelWriter new
        packageWriter: self;
        yourself ]

]

{ #category : 'accessing' }
TonelCypressWriter >> writer: aTonelWriter [
	writer := aTonelWriter

]

{ #category : 'writing' }
TonelCypressWriter >> writeSnapshot: aSnapshot [
  self
    writeSnapshot: aSnapshot
    inPackageNamed: (aSnapshot dynamicInstVarAt: #'packageName')

]

{ #category : 'writing' }
TonelCypressWriter >> writeSnapshot: aSnapshot inPackageNamed: packageName [
  snapshot := aSnapshot.	"ensure package dirs exists. 
	 It has to be just one but well..."
  self writePackage: packageName.	"now export classes"
  (self definitions select: [ :each | each isClassDefinition ])
    do: [ :each | self writer writeClass: each ].	"... and method extensions"
  self writeMethodExtensions

]

{ #category : 'writing' }
TonelCypressWriter >> writeVersion: aVersion [
	self writeSnapshot: aVersion snapshot

]
