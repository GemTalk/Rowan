Class {
	#name : 'RwComponentProjectDefinition',
	#superclass : 'RwProjectDefinition',
	#instVars : [
		'components'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwComponentProjectDefinition class >> withProperties: properties packageDefinitions: packageDefinitions componentDefinitions: componentDefinitions [

	^ self basicNew
		properties: properties;
		packages: packageDefinitions;
		components: componentDefinitions;
		yourself
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> components [

	^ components
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> components: aComponentDefinitionDictionary [

	components := aComponentDefinitionDictionary
]

{ #category : 'properties' }
RwComponentProjectDefinition >> configsPath [

	^ self projectRef configsPath
]

{ #category : 'properties' }
RwComponentProjectDefinition >> defaultConfigurationNames [

	^ self propertyAt: 'defaultConfigurationNames' ifAbsent: [ self projectRef configurationNames ]
]

{ #category : 'properties' }
RwComponentProjectDefinition >> defaultGroupNames [

	^ self propertyAt: 'defaultGroupNames' ifAbsent: [ self projectRef groupNames ]
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedConfigurationNames [

	^ self propertyAt: 'loadedConfigurationNames' ifAbsent: [ ]
]

{ #category : 'properties' }
RwComponentProjectDefinition >> loadedGroupNames [

	^ self propertyAt: 'loadedGroupNames' ifAbsent: [ ]
]

{ #category : 'accessing' }
RwComponentProjectDefinition >> projectRef [

	^ self propertyAt: 'projectRef' ifAbsent: [ ]
]

{ #category : 'tool api' }
RwComponentProjectDefinition >> readProjectSet [

	^ Rowan projectTools read  readProjectSetForComponentProjectDefinition: self
]

{ #category : 'properties' }
RwComponentProjectDefinition >> repositoryRoot [

	^ self projectRef repositoryRoot
]

{ #category : 'temporary compat' }
RwComponentProjectDefinition >> specification [

self deprecated: 'temporary method ... simulating RwSpecification api'.

	^ self
]
