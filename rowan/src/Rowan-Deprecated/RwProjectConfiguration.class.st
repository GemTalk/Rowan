"
The class RwProjectConfiguration is deprecated in favor of the class RwProjectCompoundConfiguration.

This class will be removed from the system prior to final release.
"
Class {
	#name : 'RwProjectConfiguration',
	#superclass : 'RwAbstractProjectConfiguration',
	#instVars : [
		'configNames'
	],
	#category : 'Rowan-Deprecated'
}

{ #category : 'instance creation' }
RwProjectConfiguration class >> fromSton: stonReader [

	self deprecated: 'The class RwProjectConfiguration is deprecated. Use the class RwProjectCompoundConfiguration instead'.
	^ super fromSton: stonReader

]

{ #category : 'instance creation' }
RwProjectConfiguration class >> fromUrl: specNameOrUrl [

	self deprecated: 'The class RwProjectConfiguration is deprecated. Use the class RwProjectCompoundConfiguration instead'.
	^ super fromUrl: specNameOrUrl

]

{ #category : 'instance creation' }
RwProjectConfiguration class >> newNamed: aName for: projectName [


	self deprecated: 'The class RwProjectConfiguration is deprecated. Use the class RwProjectCompoundConfiguration instead'.
	^ super newNamed: aName for: projectName

]

{ #category : 'configs' }
RwProjectConfiguration >> addConfig: configName [

	self configNames add: configName
]

{ #category : 'accessing' }
RwProjectConfiguration >> configNames [

	^ configNames ifNil: [ configNames := Set new ]
]

{ #category : 'accessing' }
RwProjectConfiguration >> configNames: anObject [

   configNames := anObject

]

{ #category : 'configs' }
RwProjectConfiguration >> removeConfig: configName [

	self configNames remove: configName ivAbsent: [  ]
]

{ #category : 'load support' }
RwProjectConfiguration >> resolvePackageNames [

	"return the list of package names that is the result of resolving all of the receiver's references"

	"all of the names in configNames, should be package names"

	^ self configNames
]

{ #category : 'load support' }
RwProjectConfiguration >> resolvePackageNames: visitedConfigurationNames [

	| thePackageNames |
	(visitedConfigurationNames includes: self name)
		ifTrue: [ ^ #() ].
	visitedConfigurationNames add: self name.
	thePackageNames := self resolvePackageNames.
	^ thePackageNames
]
