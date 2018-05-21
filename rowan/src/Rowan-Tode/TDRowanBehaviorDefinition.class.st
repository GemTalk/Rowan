Class {
	#name : 'TDRowanBehaviorDefinition',
	#superclass : 'TDBehaviorDefinition',
	#instVars : [
		'theClassDefinition'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'converting' }
TDRowanBehaviorDefinition >> asTDProtocolDefinition: protocol [

	^ (TDRowanProtocolDefinition
		className: self className
		isMeta: self isMeta
		protocol: protocol)
		appEnv: self appEnv;
		theClassDefinition: self theClassDefinition;
		yourself
]

{ #category : 'accessing' }
TDRowanBehaviorDefinition >> emphasis [

	^ self theClassDefinition emphasis
]

{ #category : 'accessing' }
TDRowanBehaviorDefinition >> theClassDefinition [

   ^theClassDefinition

]

{ #category : 'accessing' }
TDRowanBehaviorDefinition >> theClassDefinition: anObject [

   theClassDefinition := anObject

]
