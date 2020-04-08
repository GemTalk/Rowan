Extension { #name : 'RwResolvedLoadComponentsV2' }

{ #category : '*rowan-components' }
RwResolvedLoadComponentsV2 >> addComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwProjectLoadComponentV2 newNamed: aComponentName ].
	groupNameDict
		keysAndValuesDo: [ :groupName :includeGroups | component defineGroupNamed: groupName toIncludeGroups: includeGroups ].
	component comment: commentString.
	^ component
]

{ #category : '*rowan-components' }
RwResolvedLoadComponentsV2 >> addProjectNamed: projectName toComponentNamed: toComponentName withConditions: conditionArray andGroupName: groupName [
	| component |
	component := self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , toComponentName printString , ' is undefined' ].
	component
		conditionalPropertiesAtConditions: conditionArray
		andGroup: groupName
		addProjectNames: {projectName}
]
