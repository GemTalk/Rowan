Class {
	#name : 'RwProjectDefinitionV2',
	#superclass : 'RwAbstractProjectDefinitionV2',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectDefinitionV2 class >> projectName: projectName comment: commentString [
	| projectReferenceDefintion |
	projectReferenceDefintion := RwProjectReferenceDefinitionV2
		fromLoadSpecification:
			(RwProjectLoadSpecificationV2 new
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
