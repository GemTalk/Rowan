"
No class-specific documentation for CypGsPackagePropertiesPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackagePropertiesPatch( propertiesModification)

"
Class {
	#name : 'CypGsPackagePropertiesPatch',
	#superclass : 'CypGsPackagePatch',
	#instVars : [
		'propertiesModification'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'instance creation' }
CypGsPackagePropertiesPatch class >> forPackageProperties: aPropertiesModification inPackage: aPackageDefinition [

	^(self new)
		packageDefinition: aPackageDefinition;
		propertiesModification: aPropertiesModification;
		yourself
]

{ #category : 'Accessing' }
CypGsPackagePropertiesPatch >> propertiesModification [
	^propertiesModification

]

{ #category : 'Updating' }
CypGsPackagePropertiesPatch >> propertiesModification: newValue [
	propertiesModification := newValue

]
