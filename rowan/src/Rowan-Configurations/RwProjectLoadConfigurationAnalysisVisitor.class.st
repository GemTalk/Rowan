Class {
	#name : 'RwProjectLoadConfigurationAnalysisVisitor',
	#superclass : 'RwProjectLoadConfigurationVisitor',
	#instVars : [
		'visitedConfigurationMap',
		'packageToConfigurationMap'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'private' }
RwProjectLoadConfigurationAnalysisVisitor >> _addPackageNames: somePackageNames for: aConfiguration [

	super _addPackageNames: somePackageNames for: aConfiguration.
	somePackageNames do: [:aPackageName |
		(packageToConfigurationMap at: aPackageName ifAbsentPut: [ {} ])
			add: aConfiguration ]

]

{ #category : 'private' }
RwProjectLoadConfigurationAnalysisVisitor >> _visited: aConfiguration [

	super _visited: aConfiguration.
	visitedConfigurationMap at:  aConfiguration name put: aConfiguration.

]

{ #category : 'initialization' }
RwProjectLoadConfigurationAnalysisVisitor >> initialize [

	super initialize.
	visitedConfigurationMap := Dictionary new.
	packageToConfigurationMap := Dictionary new

]
