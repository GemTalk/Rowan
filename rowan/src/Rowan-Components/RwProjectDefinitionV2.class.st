Class {
	#name : 'RwProjectDefinitionV2',
	#superclass : 'RwAbstractProjectDefinitionV2',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectDefinitionV2 class >> fromLoadSpecification: aRwProjectLoadSpecificationV2 [
	| projectReferenceDefintion |
	projectReferenceDefintion := RwProjectReferenceDefinitionV2
		fromLoadSpecification: aRwProjectLoadSpecificationV2.
	^ self basicNew
		properties:
				(Dictionary new
						add: 'name' -> aRwProjectLoadSpecificationV2 projectName;
						add: 'projectRef' -> projectReferenceDefintion;
						yourself);
		packages: Dictionary new;
		yourself
]

{ #category : 'instance creation' }
RwProjectDefinitionV2 class >> projectName: projectName comment: commentString [
	| projectLoadSpecification |
	projectLoadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		yourself.
	^ (self fromLoadSpecification: projectLoadSpecification)
		comment: commentString;
		yourself
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
