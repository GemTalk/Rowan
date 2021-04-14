Class {
	#name : 'RwAbstractProject',
	#superclass : 'RowanInterface',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwAbstractProject >> _concreteProject [
	self subclassResponsibility: #'_concreteProject'
]

{ #category : 'private' }
RwAbstractProject >> _loadSpecification [
	^ self _concreteProject _loadSpecification
]

{ #category : 'properties' }
RwAbstractProject >> comment [

	^ self _concreteProject comment
]

{ #category : 'transitions' }
RwAbstractProject >> defined [
	self subclassResponsibility: #'defined'
]

{ #category : 'accessing' }
RwAbstractProject >> gemstoneDefaultSymbolDictName [
	^ self _concreteProject gemstoneDefaultSymbolDictName
]

{ #category : 'querying' }
RwAbstractProject >> gemstoneSymbolDictNameForPackageNamed: packageName [

	^ self _concreteProject gemstoneSymbolDictNameForPackageNamed: packageName
]

{ #category : 'transitions' }
RwAbstractProject >> load [
	"
		load only the receiver into the image. Required projects for the receiver are only loaded if they are not already 
			present in the image.

		To explicitly load the receiver AND required projects, construct a project set containing projects to be loaded 
			and send #load to the project set.
	"

	^ self _concreteProject load
]

{ #category : 'accessing' }
RwAbstractProject >> packageConvention [
	^ self _concreteProject packageConvention
]

{ #category : 'accessing' }
RwAbstractProject >> packageNames [
	^self _concreteProject packageNames
]

{ #category : 'accessing' }
RwAbstractProject >> platformConditionalAttributes [
	"Answer the platformConditionalAttributes that would be used to load the project"
	^ self _concreteProject platformConditionalAttributes
]

{ #category : 'accessing' }
RwAbstractProject >> project [

	^ self
]

{ #category : 'accessing' }
RwAbstractProject >> projectsHome [
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	^ self _concreteProject projectsHome
]

{ #category : 'accessing' }
RwAbstractProject >> remote [

	^ self _concreteProject remote
]

{ #category : 'accessing' }
RwAbstractProject >> repositoryRoot [
	^ self _concreteProject repositoryRoot
]
