Class {
	#name : 'RwAbstractRepositoryDefinitionTest',
	#superclass : 'RwAbstractComponentDefinitionTest',
	#category : 'Rowan-Tests'
}

{ #category : 'Testing' }
RwAbstractRepositoryDefinitionTest class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #RwAbstractRepositoryDefinitionTest
]

{ #category : 'private' }
RwAbstractRepositoryDefinitionTest >> _repositoryDefinitionClass [

	self subclassResponsibility: #_repositoryDefinitionClass
]

{ #category : 'tests' }
RwAbstractRepositoryDefinitionTest >> testRepositoryCreation [

	| repoDef |
	repoDef := self _repositoryDefinitionClass 
		newNamed: 'RowanTestRepository' 
			repositoryRoot: '/repos/Example'
			specsPath: 'rowan/specs'
			configsPath: 'rowan/configs'
			srcPath: 'rowan/src'.
]
