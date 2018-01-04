Class {
	#name : 'RwGemStoneSpecification',
	#superclass : 'RwPlatformSpecification',
	#instVars : [
		'symbolDictName',
		'userId',
		'methodEnv'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'initialization' }
RwGemStoneSpecification >> initialize [

	super initialize.
	symbolDictName := 'UserGlobals'.

]

{ #category : 'accessing' }
RwGemStoneSpecification >> methodEnv [

   ^methodEnv ifNil: [ methodEnv := 0 ]

]

{ #category : 'accessing' }
RwGemStoneSpecification >> methodEnv: anObject [

   methodEnv := anObject

]

{ #category : 'accessing' }
RwGemStoneSpecification >> symbolDictName [

   ^symbolDictName

]

{ #category : 'accessing' }
RwGemStoneSpecification >> symbolDictName: anObject [

   symbolDictName := anObject

]

{ #category : 'accessing' }
RwGemStoneSpecification >> userId [

   ^userId

]

{ #category : 'accessing' }
RwGemStoneSpecification >> userId: anObject [

   userId := anObject

]
