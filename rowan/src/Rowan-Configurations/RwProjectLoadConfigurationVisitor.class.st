Class {
	#name : 'RwProjectLoadConfigurationVisitor',
	#superclass : 'RwProjectConfigurationVisitor',
	#instVars : [
		'platformAttributes',
		'groupNames'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visitProjectLoadConfiguration: aProjecLoadConfiguration [

	(visitedConfigurationNames includes: aProjecLoadConfiguration name)
		ifTrue: [ ^ self ].

	visitedConfigurationNames add:  aProjecLoadConfiguration name. 

self halt.

]
