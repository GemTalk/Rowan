Class {
	#name : 'RwProjectDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'packageSet'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwProjectDefinition class >> newNamed: packageName [

	^ self
		withProperties: (Dictionary with: 'name' -> packageName)
		packageSet: RwPackageSetDefinition new
]

{ #category : 'instance creation' }
RwProjectDefinition class >> withProperties: properties packageSet: packageSetDefinition [

	^ self basicNew
		properties: properties;
		packageSet: packageSetDefinition;
		yourself
]

{ #category : 'initialization' }
RwProjectDefinition >> initialize [

	super initialize.
	packageSet := RwPackageSetDefinition new
]

{ #category : 'properties' }
RwProjectDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self propertyAt: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwProjectDefinition >> packageSet [

   ^packageSet

]

{ #category : 'accessing' }
RwProjectDefinition >> packageSet: anObject [

   packageSet := anObject

]
