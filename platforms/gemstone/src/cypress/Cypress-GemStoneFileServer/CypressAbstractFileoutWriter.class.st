"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressAbstractFileoutWriter',
	#superclass : 'CypressAbstractPackageFiler',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'accessing' }
CypressAbstractFileoutWriter >> classesInDependencyOrder [

	^(CypressGsGeneralDependencySorter
		on: self packageStructure classes
		dependsOn: [:candidate | candidate superclassName]
		dependent: [:candidate | candidate className]) inOrder

]

{ #category : 'accessing' }
CypressAbstractFileoutWriter >> classesWithInitializers [

	^self classesInDependencyOrder
		select: [:each | each classMethods anySatisfy: [:method | method selector = 'initialize']]

]

{ #category : 'private' }
CypressAbstractFileoutWriter >> determinePackageDirectory [

	^self fileUtils ensureDirectoryExists: self repository directoryPath

]

{ #category : 'accessing' }
CypressAbstractFileoutWriter >> extensions [

	^self packageStructure extensions

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOut: aString implementationsFrom: someClassStructures on: aStream [

	someClassStructures
		do: [:each | self fileOutType: aString implementationOf: each on: aStream]

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOut: aString methods: someMethodStructures on: aStream [

	someMethodStructures isEmpty ifTrue: [^self].
	self
		fileOut: aString methodsPreambleFor: someMethodStructures any classStructure on: aStream;
		fileOutMethods: someMethodStructures on: aStream

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOutClassDeclarationsOn: aStream [

	self classesInDependencyOrder
		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOutClassesOn: aStream [

	self
		fileOutClassesPreambleOn: aStream;
		fileOutClassDeclarationsOn: aStream;
		fileOutClassImplementationsOn: aStream

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOutClassImplementationsOn: aStream [

	self
		fileOut: 'Class Implementation'
		implementationsFrom: self classesInDependencyOrder
		on: aStream

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOutClassInitializersOn: aStream [

	self fileOutClassInitializersPreambleOn: aStream.
	self classesWithInitializers do: [:each | self fileOutClassInitializerFor: each on: aStream].
	self fileOutClassInitializersPostambleOn: aStream.

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOutExtensionImplementationsOn: aStream [
  | sortedExtensions |
  sortedExtensions := self extensions
    asSortedCollection: [ :a :b | a className <= b className ].
  self
    fileOut: 'Class Extension'
    implementationsFrom: sortedExtensions
    on: aStream

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOutExtensionsOn: aStream [

	self
		fileOutExtensionsPreambleOn: aStream;
		fileOutExtensionImplementationsOn: aStream

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOutMethods: someMethodStructures on: aStream [

	(someMethodStructures
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:methodStructure | self fileOutMethod: methodStructure on: aStream]

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOutPackageOn: aStream [

	self
		fileOutPackagePreambleOn: aStream;
		fileOutClassesOn: aStream;
		fileOutExtensionsOn: aStream;
		fileOutClassInitializersOn: aStream;
		fileOutPackagePostambleOn: aStream

]

{ #category : 'writing - private' }
CypressAbstractFileoutWriter >> fileOutType: aString implementationOf: classStructure on: aStream [

	self
		fileOutPreambleType: aString
			for: classStructure
			on: aStream;
		fileOut: 'Class'
			methods: classStructure classMethods
			on: aStream;
		fileOut: 'Instance'
			methods: classStructure instanceMethods
			on: aStream

]

{ #category : 'accessing' }
CypressAbstractFileoutWriter >> packageName [

	^self packageStructure packageName

]

{ #category : 'accessing' }
CypressAbstractFileoutWriter >> packageNameExtension [

	^self subclassResponsibility: #packageNameExtension

]

{ #category : 'writing' }
CypressAbstractFileoutWriter >> writePackageStructure [

	CypressFileUtilities current
		writeStreamFor: self packageStructure packageName
				, self packageNameExtension
		in: self packageDirectory
		do: [:fileStream | self fileOutPackageOn: fileStream]

]

{ #category : 'writing' }
CypressAbstractFileoutWriter >> writePackageStructure: aPackageStructure [

	self
		packageStructure: aPackageStructure;
		packageDirectory: self determinePackageDirectory;
		writePackageStructure

]
