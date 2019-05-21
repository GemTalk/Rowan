"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressDictionaryRepository',
	#superclass : 'CypressAbstractRepository',
	#instVars : [
		'dictionary'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'instance creation' }
CypressDictionaryRepository class >> on: aDictionary [
  ^ self new
    initializeForDictionary: aDictionary;
    yourself

]

{ #category : 'accessing' }
CypressDictionaryRepository >> dictionary [
  ^ dictionary

]

{ #category : 'accessing' }
CypressDictionaryRepository >> dictionary: aDictionary [
  dictionary := aDictionary

]

{ #category : 'initializing - private' }
CypressDictionaryRepository >> initialize [
  super initialize.
  readerClass := nil.
  writerClass := nil

]

{ #category : 'initializing - private' }
CypressDictionaryRepository >> initializeForDictionary: aDictionary [
  self initialize.
  self dictionary: aDictionary

]

{ #category : 'accessing' }
CypressDictionaryRepository >> packageNames [
  ^ self dictionary keys

]

{ #category : 'reading' }
CypressDictionaryRepository >> readPackageStructureForPackageNamed: packageName [
  ^ (self dictionary at: packageName) packageStructure

]

{ #category : 'writing' }
CypressDictionaryRepository >> writePackageStructure: aPackageStructure [
  ^ self dictionary at: aPackageStructure packageName put: aPackageStructure

]
