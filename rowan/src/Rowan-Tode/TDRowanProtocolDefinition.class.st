Class {
	#name : 'TDRowanProtocolDefinition',
	#superclass : 'TDProtocolDefinition',
	#instVars : [
		'theClassDefinition'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'converting' }
TDRowanProtocolDefinition >> asTDClassDefinition [

	^ self theClassDefinition
]

{ #category : 'accessing' }
TDRowanProtocolDefinition >> theClassDefinition [

   ^theClassDefinition

]

{ #category : 'accessing' }
TDRowanProtocolDefinition >> theClassDefinition: anObject [

   theClassDefinition := anObject

]
