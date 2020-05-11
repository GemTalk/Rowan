Class {
	#name : 'RwAbstractUnloadedProject',
	#superclass : 'RwAbstractProject',
	#instVars : [
		'resolvedProject'
	],
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwAbstractUnloadedProject >> _loadSpecification [
	^ self _resolvedProject _loadSpecification
]

{ #category : 'private' }
RwAbstractUnloadedProject >> _resolvedProject [
	^ resolvedProject
]

{ #category : 'private' }
RwAbstractUnloadedProject >> _resolvedProject: aResolvedProject [
	resolvedProject := aResolvedProject
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> comment: aString [
	self _resolvedProject comment: aString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self _resolvedProject gemstoneSetDefaultSymbolDictNameTo: symbolDictName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gitUrl: aString [
	self _loadSpecification gitUrl: aString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> packageNames [
	^self _resolvedProject packageNames
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> projectsHome [
	^self _resolvedProject projectsHome
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> projectsHome: aProjectHomeReferenceOrString [
	self _resolvedProject projectsHome: aProjectHomeReferenceOrString
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
RwAbstractUnloadedProject >> specComment: aString [
	self _loadSpecification comment: aString
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

	^self _resolvedProject useGit
]
