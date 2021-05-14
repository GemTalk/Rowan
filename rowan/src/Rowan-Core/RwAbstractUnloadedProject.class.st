Class {
	#name : 'RwAbstractUnloadedProject',
	#superclass : 'RwAbstractProject',
	#instVars : [
		'concreteProject'
	],
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwAbstractUnloadedProject >> _concreteProject [
	^ concreteProject
]

{ #category : 'private' }
RwAbstractUnloadedProject >> _concreteProject: aResolvedProject [
	concreteProject := aResolvedProject
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> comment: aString [
	self _concreteProject comment: aString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> diskUrl: aString [
	self _loadSpecification diskUrl: aString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self _concreteProject gemstoneSetDefaultSymbolDictNameTo: symbolDictName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gitUrl: aString [
	self _loadSpecification gitUrl: aString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> packageFormat: aString [
	^ self _concreteProject packageFormat: aString
]

{ #category : 'actions' }
RwAbstractUnloadedProject >> packages [
	^ self _concreteProject packages
]

{ #category : 'actions' }
RwAbstractUnloadedProject >> packages: aPackageDictionary [
	self _concreteProject packages: aPackageDictionary
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> repositoryResolutionPolicy [
	^ self _loadSpecification repositoryResolutionPolicy
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> repositoryResolutionPolicy: aSymbolOrNil [
	self _loadSpecification repositoryResolutionPolicy: aSymbolOrNil
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> specComponentNames: anArray [
	self _loadSpecification componentNames: anArray
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> specName [
	^ self _loadSpecification specName
]

{ #category : 'testing' }
RwAbstractUnloadedProject >> useGit [

	^self _concreteProject useGit
]
