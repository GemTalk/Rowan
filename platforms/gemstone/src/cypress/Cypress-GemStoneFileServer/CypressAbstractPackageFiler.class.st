"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressAbstractPackageFiler',
	#superclass : 'Object',
	#instVars : [
		'repository',
		'packageDirectory',
		'packageStructure'
	],
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'instance creation' }
CypressAbstractPackageFiler class >> forRepository: aCypressFileSystemRepository [

	^self new
		initializeForRepository: aCypressFileSystemRepository;
		yourself.

]

{ #category : 'private' }
CypressAbstractPackageFiler >> fileUtils [

	^CypressFileUtilities current

]

{ #category : 'initializing - private' }
CypressAbstractPackageFiler >> initializeForRepository: aCypressFileSystemRepository [

	repository := aCypressFileSystemRepository

]

{ #category : 'accessing' }
CypressAbstractPackageFiler >> packageDirectory [

	^packageDirectory

]

{ #category : 'accessing' }
CypressAbstractPackageFiler >> packageDirectory: aDirectory [

	packageDirectory := aDirectory

]

{ #category : 'accessing' }
CypressAbstractPackageFiler >> packageStructure [

	^packageStructure

]

{ #category : 'accessing' }
CypressAbstractPackageFiler >> packageStructure: aPackageStructure [

	packageStructure := aPackageStructure

]

{ #category : 'accessing' }
CypressAbstractPackageFiler >> propertiesFileNameExtension [

	^'.ston'

]

{ #category : 'accessing' }
CypressAbstractPackageFiler >> propertiesFileNameExtensions [

	^Array with: self propertiesFileNameExtension

]

{ #category : 'accessing' }
CypressAbstractPackageFiler >> repository [

	^repository

]

{ #category : 'private' }
CypressAbstractPackageFiler >> string: aString endsWith: subString [
	"Answer whether the last characters of aString are the same as subString."

	| expectedStart |
	expectedStart := aString size - subString size + 1 max: 1.
	^expectedStart
		= (aString indexOfSubCollection: subString startingAt: expectedStart)

]
