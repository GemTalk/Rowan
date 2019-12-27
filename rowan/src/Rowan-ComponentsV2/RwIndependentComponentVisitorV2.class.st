Class {
	#name : 'RwIndependentComponentVisitorV2',
	#superclass : 'RwAbstractProjectComponentVisitorV2',
	#instVars : [
		'packageNames',
		'componentsRoot',
		'projectsRoot'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'private' }
RwIndependentComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self packageNames addAll: somePackageNames

]

{ #category : 'accessing' }
RwIndependentComponentVisitorV2 >> componentsRoot [
	^ componentsRoot
]

{ #category : 'accessing' }
RwIndependentComponentVisitorV2 >> componentsRoot: aStringOrFileReference [
	componentsRoot := aStringOrFileReference asFileReference
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
RwIndependentComponentVisitorV2 >> projectsRoot [
	^ projectsRoot
]

{ #category : 'accessing' }
RwIndependentComponentVisitorV2 >> projectsRoot: aString [
	projectsRoot := aString asFileReference
]
