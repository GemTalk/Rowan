Class {
	#name : 'RwIndependentComponentVisitorV2',
	#superclass : 'RwAbstractProjectComponentVisitorV2',
	#instVars : [
		'packageNames',
		'componentsPath',
		'projectsPath'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'private' }
RwIndependentComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self packageNames addAll: somePackageNames

]

{ #category : 'accessing' }
RwIndependentComponentVisitorV2 >> componentsPath [
	^ componentsPath
]

{ #category : 'accessing' }
RwIndependentComponentVisitorV2 >> componentsPath: aString [
	componentsPath := aString
]

{ #category : 'initialization' }
RwIndependentComponentVisitorV2 >> initialize [
	super initialize.
	packageNames := Set new.
]

{ #category : 'accessing' }
RwIndependentComponentVisitorV2 >> packageNames [

	^ packageNames
]

{ #category : 'accessing' }
RwIndependentComponentVisitorV2 >> projectsPath [
	^ projectsPath
]

{ #category : 'accessing' }
RwIndependentComponentVisitorV2 >> projectsPath: aString [
	projectsPath := aString
]
