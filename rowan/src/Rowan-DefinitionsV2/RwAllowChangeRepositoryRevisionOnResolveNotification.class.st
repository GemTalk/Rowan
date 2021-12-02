Class {
	#name : 'RwAllowChangeRepositoryRevisionOnResolveNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'loadSpecification'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'Instance creation' }
RwAllowChangeRepositoryRevisionOnResolveNotification class >> signal: aRwProjectLoadSpecificationV2 [
	"An exception of the type associated with the receiver is signaled."

	^ self new
		loadSpecification: aRwProjectLoadSpecificationV2;
		signal
]

{ #category : 'Handling' }
RwAllowChangeRepositoryRevisionOnResolveNotification >> defaultAction [
	"By default allow revisions to be changed when resolving a load specification"

	^ false
]

{ #category : 'accessing' }
RwAllowChangeRepositoryRevisionOnResolveNotification >> loadSpecification [
	^loadSpecification
]

{ #category : 'accessing' }
RwAllowChangeRepositoryRevisionOnResolveNotification >> loadSpecification: object [
	loadSpecification := object
]
