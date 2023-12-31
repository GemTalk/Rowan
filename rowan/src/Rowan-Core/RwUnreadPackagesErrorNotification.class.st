Class {
	#name : 'RwUnreadPackagesErrorNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'errorMessage',
		'unreadPackageNames'
	],
	#category : 'Rowan-Core'
}

{ #category : 'handling' }
RwUnreadPackagesErrorNotification >> defaultAction [

	self error: self errorMessage
]

{ #category : 'accessing' }
RwUnreadPackagesErrorNotification >> errorMessage [

   ^errorMessage

]

{ #category : 'accessing' }
RwUnreadPackagesErrorNotification >> errorMessage: anObject [

   errorMessage := anObject

]

{ #category : 'accessing' }
RwUnreadPackagesErrorNotification >> unreadPackageNames [
	^unreadPackageNames
]

{ #category : 'accessing' }
RwUnreadPackagesErrorNotification >> unreadPackageNames: object [
	unreadPackageNames := object
]
