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
RwAbstractUnloadedProject >> gemstoneDefaultSymbolDictName [
	^ self _concreteProject gemstoneDefaultSymbolDictName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self _concreteProject gemstoneSetDefaultSymbolDictNameTo: symbolDictName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gitUrl: aString [
	self _loadSpecification gitUrl: aString
]

{ #category : 'transitions' }
RwAbstractUnloadedProject >> load [
	"
		load only the receiver into the image. Required projects for the receiver are only loaded if they are not already 
			present in the image.

		To explicitly load the receiver AND required projects, construct a project set containing projects to be loaded 
			and send #load to the project set.
	"

	^ self _concreteProject load
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> packageConvention [
	^ self _concreteProject packageConvention
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> packageFormat: aString [
	^ self _concreteProject packageFormat: aString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> packageNames [
	^self _concreteProject packageNames
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
RwAbstractUnloadedProject >> platformConditionalAttributes [
	"Answer the platformConditionalAttributes that would be used to load the project"
	^ self _concreteProject platformConditionalAttributes
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> projectsHome [
	^self _concreteProject projectsHome
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> projectsHome: aProjectHomeReferenceOrString [
	self _concreteProject projectsHome: aProjectHomeReferenceOrString
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> remote [

	^ self _concreteProject remote
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

	^self _concreteProject useGit
]
