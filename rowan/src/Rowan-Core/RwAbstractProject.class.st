Class {
	#name : 'RwAbstractProject',
	#superclass : 'RowanInterface',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwAbstractProject >> _concreteProject [
	self subclassResponsibility: #'_concreteProject'
]

{ #category : 'properties' }
RwAbstractProject >> comment [

	^ self _concreteProject comment
]

{ #category : 'accessing' }
RwAbstractProject >> customConditionalAttributes [
	"Answer the customConditionalAttributes that would be used to load the project"
	^ self _concreteProject customConditionalAttributes
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

{ #category : 'accessing' }
RwAbstractProject >> loadSpecification [
	^ self _concreteProject loadSpecification
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
RwAbstractProject >> projectName [
	^ self name
]

{ #category : 'accessing' }
RwAbstractProject >> projectsHome [
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	^ self _concreteProject projectsHome
]

{ #category : 'accessing' }
RwAbstractProject >> projectsHome: aProjectHomeReferenceOrString [
	self _concreteProject projectsHome: aProjectHomeReferenceOrString
]

{ #category : 'accessing' }
RwAbstractProject >> projectVersion [
	^ self _concreteProject projectVersion
]

{ #category : 'accessing' }
RwAbstractProject >> projectVersion: aStringOrVersion [
	^ self _concreteProject projectVersion: aStringOrVersion
]

{ #category : 'accessing' }
RwAbstractProject >> relativeRepositoryRoot [
	"return the repository root relative to the git repository root ... not applicable to non-git (svn, etc.) repositories"

	^ self loadSpecification relativeRepositoryRoot
]

{ #category : 'accessing' }
RwAbstractProject >> remote [

	^ self _concreteProject remote
]

{ #category : 'querying' }
RwAbstractProject >> requiredProjectNames [
	^ self _concreteProject requiredProjectNames
]
