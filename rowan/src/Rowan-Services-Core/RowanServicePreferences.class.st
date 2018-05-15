Class {
	#name : 'RowanServicePreferences',
	#superclass : 'Object',
	#instVars : [
		'defaultProjectName'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanServicePreferences class >> current [
	^(System myUserProfile objectNamed: 'UserGlobals') 
		at: #RowanServicePrefs 
		ifAbsentPut: [ self new ]

]

{ #category : 'accessing' }
RowanServicePreferences >> defaultProjectName [

	^defaultProjectName

]

{ #category : 'accessing' }
RowanServicePreferences >> defaultProjectName: aString [

	defaultProjectName := aString

]
