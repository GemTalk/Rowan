Class {
	#name : 'RwProjectDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'packages'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectDefinitionV2 class >> projectName: projectName comment: commentString [
	| projectReferenceDefintion |
	projectReferenceDefintion := RwProjectReferenceDefinitionV2
		fromLoadSpecification:
			(RwProjectLoadSpecification new
				projectName: projectName;
				yourself).
	^ self basicNew
		properties:
				(Dictionary new
						add: 'name' -> projectName;
						add: 'projectRef' -> projectReferenceDefintion;
						yourself);
		packages: Dictionary new;
		comment: commentString;
		yourself
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> comment [

	^ self projectRef comment
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> comment: aString [

	self projectRef comment: aString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self projectRef projectName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> packages [

	^packages
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> packages: aPackageDefinitionDictionary [

	packages := aPackageDefinitionDictionary
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectLoadSpecification [
	^ self projectRef projectLoadSpecification
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectName [ 

	^ self projectRef projectName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectRef [
	^ self propertyAt: 'projectRef'
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectRef: aRwProjectReferenceDefinitionV2 [

	^ self propertyAt: 'projectRef' put: aRwProjectReferenceDefinitionV2
]
