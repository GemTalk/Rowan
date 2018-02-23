"
Abstract. Responsible for reading or writing a disk format of a package definition.
"
Class {
	#name : 'CypPackageIO',
	#superclass : 'Object',
	#instVars : [
		'repository',
		'packageDirectory',
		'disk'
	],
	#category : 'Cypress-Core'
}

{ #category : 'Updating' }
CypPackageIO >> disk: newValue [
	disk := newValue

]
