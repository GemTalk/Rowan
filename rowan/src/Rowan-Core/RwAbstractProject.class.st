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
RwAbstractProject >> componentNamed: componentName [
	^ self _concreteProject componentNamed: componentName
]

{ #category : 'accessing' }
RwAbstractProject >> componentNames [
	"list of component names from the load specification used to load the project "

	^ self _concreteProject componentNames
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
RwAbstractProject >> diskUrl [
	^ self _concreteProject diskUrl
]

{ #category : 'accessing' }
RwAbstractProject >> gemstoneDefaultMethodEnv [
	^ self _concreteProject gemstoneDefaultMethodEnv
]

{ #category : 'accessing' }
RwAbstractProject >> gemstoneDefaultMethodEnvForUser: userId [
	^ self _concreteProject gemstoneDefaultMethodEnvForUser: userId
]

{ #category : 'accessing' }
RwAbstractProject >> gemstoneDefaultSymbolDictName [
	^ self _concreteProject gemstoneDefaultSymbolDictName
]

{ #category : 'accessing' }
RwAbstractProject >> gemstoneDefaultSymbolDictNameForUser: userId [
	^ self _concreteProject gemstoneDefaultSymbolDictNameForUser: userId
]

{ #category : 'accessing' }
RwAbstractProject >> gemstoneDefaultUseSessionMethodsForExtensions [
	^ self _concreteProject gemstoneDefaultUseSessionMethodsForExtensions
]

{ #category : 'accessing' }
RwAbstractProject >> gemstoneDefaultUseSessionMethodsForExtensionsForUser: userId [
	^ self _concreteProject
		gemstoneDefaultUseSessionMethodsForExtensionsForUser: userId
]

{ #category : 'querying' }
RwAbstractProject >> gemstoneMethodEnvForPackageNamed: packageName [

	^ self _concreteProject gemstoneMethodEnvForPackageNamed: packageName
]

{ #category : 'querying' }
RwAbstractProject >> gemstoneMethodEnvForPackageNamed: packageName forUser: userId [
	^ self _concreteProject
		gemstoneMethodEnvForPackageNamed: packageName
		forUser: userId
]

{ #category : 'querying' }
RwAbstractProject >> gemstoneSymbolDictNameForPackageNamed: packageName [

	^ self _concreteProject gemstoneSymbolDictNameForPackageNamed: packageName
]

{ #category : 'querying' }
RwAbstractProject >> gemstoneSymbolDictNameForPackageNamed: packageName forUser: userId [
	^ self _concreteProject
		gemstoneSymbolDictNameForPackageNamed: packageName
		forUser: userId
]

{ #category : 'querying' }
RwAbstractProject >> gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName [
	^ self _concreteProject
		gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName
]

{ #category : 'querying' }
RwAbstractProject >> gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName forUser: userId [
	^ self _concreteProject
		gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName
		forUser: userId
]

{ #category : 'accessing' }
RwAbstractProject >> gitRoot [
	^ self _concreteProject gitRoot
]

{ #category : 'accessing' }
RwAbstractProject >> gitUrl [
	^ self _concreteProject gitUrl
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
RwAbstractProject >> projectAlias [
	^ self loadSpecification projectAlias
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
RwAbstractProject >> readOnlyDiskUrl [
	^ self _concreteProject readOnlyDiskUrl
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
