"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressAbstractRepository',
	#superclass : 'Object',
	#instVars : [
		'url',
		'properties',
		'readerClass',
		'writerClass'
	],
	#classVars : [
		'DefaultCopyrightNotice'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'instance creation' }
CypressAbstractRepository class >> createOn: aUrl alias: aString [
  ^ self onUrl: aUrl alias: aString

]

{ #category : 'accessing' }
CypressAbstractRepository class >> defaultCopyrightNotice [

	^DefaultCopyrightNotice

]

{ #category : 'accessing' }
CypressAbstractRepository class >> defaultCopyrightNotice: aString [

	DefaultCopyrightNotice := aString

]

{ #category : 'initializing' }
CypressAbstractRepository class >> initialize [

	self initializeDefaultCopyrightNotice

]

{ #category : 'initializing' }
CypressAbstractRepository class >> initializeDefaultCopyrightNotice [

	self defaultCopyrightNotice isNil ifFalse: [^self].
	self defaultCopyrightNotice: 'This work is protected by copyright. All rights reserved.'

]

{ #category : 'instance creation' }
CypressAbstractRepository class >> onUrl: aUrl alias: aString [

	^(aUrl repositoryClass new)
		initializeUrl: aUrl andAlias: aString;
		yourself

]

{ #category : 'accessing properties' }
CypressAbstractRepository >> alias [

	^properties 
		at: 'alias'
		ifAbsent: ['']

]

{ #category : 'accessing properties' }
CypressAbstractRepository >> alias: aString [

	properties 
		at: 'alias'
		put: aString

]

{ #category : 'accessing properties' }
CypressAbstractRepository >> copyrightProperty [

	^properties 
		at: '_cypress_copyright'
		ifAbsent: ['']

]

{ #category : 'accessing properties' }
CypressAbstractRepository >> copyrightProperty: aString [

	properties 
		at: '_cypress_copyright'
		put: aString

]

{ #category : 'accessing' }
CypressAbstractRepository >> defaultCopyrightNotice [

	^self class defaultCopyrightNotice

]

{ #category : 'accessing' }
CypressAbstractRepository >> description [

	^self alias

]

{ #category : 'initializing - private' }
CypressAbstractRepository >> initialize [

	self initializeDefaultRepositoryProperties.
	readerClass := CypressPackageReader.
	writerClass := CypressPackageWriter.

]

{ #category : 'initializing - private' }
CypressAbstractRepository >> initializeDefaultCopyrightProperty [

	self copyrightProperty: self defaultCopyrightNotice

]

{ #category : 'initializing - private' }
CypressAbstractRepository >> initializeDefaultRepositoryProperties [

	properties := Dictionary new.
	self initializeDefaultCopyrightProperty

]

{ #category : 'initializing - private' }
CypressAbstractRepository >> initializeUrl: aUrl andAlias: aString [

	self
		initialize;
		url: aUrl;
		alias: aString;
		validateUrl

]

{ #category : 'accessing properties' }
CypressAbstractRepository >> packageExtension [

	^properties 
		at: 'packageExtension'
		ifAbsent: ['.package']

]

{ #category : 'printing' }
CypressAbstractRepository >> printDetailsOn: aStream [

	aStream nextPutAll: self alias

]

{ #category : 'printing' }
CypressAbstractRepository >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'

]

{ #category : 'reading' }
CypressAbstractRepository >> reader [

	^readerClass forRepository: self

]

{ #category : 'reading' }
CypressAbstractRepository >> readPackageStructureForPackageNamed: packageName [

	^(self reader)
		readPackageStructureForPackageNamed: packageName;
		packageStructure

]

{ #category : 'accessing' }
CypressAbstractRepository >> url [

	^url

]

{ #category : 'accessing' }
CypressAbstractRepository >> url: aString [

	url := aString

]

{ #category : 'validating - private' }
CypressAbstractRepository >> validateUrl [
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

]

{ #category : 'writing' }
CypressAbstractRepository >> writePackageStructure: aPackageStructure [

	^self writer writePackageStructure: aPackageStructure

]

{ #category : 'writing' }
CypressAbstractRepository >> writer [

	^writerClass forRepository: self

]
