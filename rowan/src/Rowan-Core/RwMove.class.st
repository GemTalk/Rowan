Class {
	#name : 'RwMove',
	#superclass : 'Object',
	#instVars : [
		'projectBefore',
		'projectAfter',
		'packageBefore',
		'packageAfter'
	],
	#category : 'Rowan-Core'
}

{ #category : 'Accessing' }
RwMove >> packageAfter [
	^packageAfter

]

{ #category : 'Updating' }
RwMove >> packageAfter: newValue [
	packageAfter := newValue

]

{ #category : 'Accessing' }
RwMove >> packageBefore [
	^packageBefore

]

{ #category : 'Updating' }
RwMove >> packageBefore: newValue [
	packageBefore := newValue

]

{ #category : 'Accessing' }
RwMove >> projectAfter [
	^projectAfter

]

{ #category : 'Updating' }
RwMove >> projectAfter: newValue [
	projectAfter := newValue

]

{ #category : 'Accessing' }
RwMove >> projectBefore [
	^projectBefore

]

{ #category : 'Updating' }
RwMove >> projectBefore: newValue [
	projectBefore := newValue

]

{ #category : 'Accessing' }
RwMove >> symbolDictionaryNameAfter [
	^ self projectAfter
		gemstoneSymbolDictNameForPackageNamed: self packageAfter name
]

{ #category : 'Accessing' }
RwMove >> symbolDictionaryNameBefore [
	^ self projectBefore
		gemstoneSymbolDictNameForPackageNamed: self packageBefore name
]
