"
This notification provides access to a load spec for a required project that needs to be customized. The notification is signalled from RwLoadSpecificationV2 >> resolveProject before the project is cloned, and before the project is read from disk. Especially useful if the load specs of required projects need to modified before cloning (i.e., adjusting the revision to be used or the giturl to use for cloning).
"
Class {
	#name : 'RwAdjustLoadSpecBeforeProjectCloneOrLoadNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'loadSpecification'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwAdjustLoadSpecBeforeProjectCloneOrLoadNotification class >> for: aLoadSpecification [
	^ self new
		loadSpecification: aLoadSpecification;
		yourself
]

{ #category : 'handling' }
RwAdjustLoadSpecBeforeProjectCloneOrLoadNotification >> defaultAction [
	"return the adjusted  loadSpecification"

	^ loadSpecification
]

{ #category : 'accessing' }
RwAdjustLoadSpecBeforeProjectCloneOrLoadNotification >> loadSpecification [
	^loadSpecification
]

{ #category : 'accessing' }
RwAdjustLoadSpecBeforeProjectCloneOrLoadNotification >> loadSpecification: object [
	loadSpecification := object
]
