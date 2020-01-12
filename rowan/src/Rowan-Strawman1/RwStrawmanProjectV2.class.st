Class {
	#name : 'RwStrawmanProjectV2',
	#superclass : 'RwAbstractResolvedObjectV2',
	#instVars : [
		'loadComponents',
		'comment'
	],
	#category : 'Rowan-Strawman1'
}

{ #category : 'private' }
RwStrawmanProjectV2 >> _loadComponents [
	"load component should not be accessed directly -- Rowan private state"

	^ loadComponents
]

{ #category : 'private' }
RwStrawmanProjectV2 >> _loadComponents: aRwResolvedLoadComponentsV2 [
	"load component should not be accessed directly -- Rowan private state"

	loadComponents := aRwResolvedLoadComponentsV2
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> addPackagesNamed: packageNames toComponentNamed: aComponentName [
	self _loadComponents
		addPackagesNamed: packageNames
		toComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> addSimpleComponentNamed: aComponentName comment: commentString [
	^ self _loadComponents
		addSimpleComponentNamed: aComponentName
		comment: commentString
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> comment [
	^ comment
]

{ #category : 'accessing' }
RwStrawmanProjectV2 >> comment: aString [
	comment := aString
]

{ #category : 'initiailization' }
RwStrawmanProjectV2 >> initialize [
	super initialize.
	loadComponents := RwResolvedLoadComponentsV2 new
]

{ #category : 'package properties' }
RwStrawmanProjectV2 >> vastSetVaPrequisitesTo: prereqArray forPackageNamed: packageName [
	self _loadComponents
		vastSetVaPrequisitesTo: prereqArray
		forPackageNamed: packageName
]

{ #category : 'package properties' }
RwStrawmanProjectV2 >> vastVaPrequisitesForPackageNamed: packageName [
	^ self _loadComponents vastVaPrequisitesForPackageNamed: packageName
]
