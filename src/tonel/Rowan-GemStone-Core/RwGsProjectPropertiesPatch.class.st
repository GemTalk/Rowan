"
No class-specific documentation for CypGsPackagePropertiesPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsPackagePropertiesPatch( propertiesModification)

"
Class {
	#name : 'RwGsProjectPropertiesPatch',
	#superclass : 'RwGsProjectPatch',
	#instVars : [
		'propertiesModification'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
RwGsProjectPropertiesPatch class >> forProjectProperties: aPropertiesModification inProject: aProjectDefinition [

	^ self new
		projectDefinition: aProjectDefinition;
		propertiesModification: aPropertiesModification;
		yourself
]

{ #category : 'Accessing' }
RwGsProjectPropertiesPatch >> propertiesModification [
	^propertiesModification

]

{ #category : 'Updating' }
RwGsProjectPropertiesPatch >> propertiesModification: newValue [
	propertiesModification := newValue

]
