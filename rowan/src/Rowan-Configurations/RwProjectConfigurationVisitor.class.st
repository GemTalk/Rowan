Class {
	#name : 'RwProjectConfigurationVisitor',
	#superclass : 'Object',
	#instVars : [
		'packageNames',
		'visitedConfigurationNames'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'instance creation' }
RwProjectConfigurationVisitor class >> new [

	^super new initialize

]

{ #category : 'initialization' }
RwProjectConfigurationVisitor >> initialize [

	packageNames := Set new.
	visitedConfigurationNames := Set new

]

{ #category : 'accessing' }
RwProjectConfigurationVisitor >> packageNames [

	^ packageNames

]

{ #category : 'public' }
RwProjectConfigurationVisitor >> visit: aProjectConfiguration [

	^aProjectConfiguration acceptVisitor: self

]

{ #category : 'visiting' }
RwProjectConfigurationVisitor >> visitProjectCompoundConfiguration: aProjectCompoundConfiguration [

	(visitedConfigurationNames includes: aProjectCompoundConfiguration name)
		ifTrue: [ ^ self ].

	visitedConfigurationNames add:  aProjectCompoundConfiguration name. 
	packageNames addAll: aProjectCompoundConfiguration packageNames.

	aProjectCompoundConfiguration _configurations
		do: [:config | config acceptVisitor: self ].

]
