"
No class-specific documentation for CypGsPackagePropertiesPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackagePropertiesPatch( propertiesModification)

"
Class {
	#name : 'RwGsPackagePropertiesPatch',
	#superclass : 'RwGsPackagePatch',
	#instVars : [
		'propertiesModification'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
RwGsPackagePropertiesPatch class >> forPackageProperties: aPropertiesModification inPackage: aPackageDefinition [

	^(self new)
		packageDefinition: aPackageDefinition;
		propertiesModification: aPropertiesModification;
		yourself
]

{ #category : 'Accessing' }
RwGsPackagePropertiesPatch >> propertiesModification [
	^propertiesModification

]

{ #category : 'Updating' }
RwGsPackagePropertiesPatch >> propertiesModification: newValue [
	propertiesModification := newValue

]
