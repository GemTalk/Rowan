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
	^(RwPlatform _userPlatformDictionary) 
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
