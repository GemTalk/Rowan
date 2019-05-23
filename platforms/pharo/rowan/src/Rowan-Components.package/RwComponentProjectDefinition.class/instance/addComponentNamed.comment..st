accessing
addComponentNamed: aComponentName comment: commentString

	| component |
	component := self 
		components at: aComponentName 
		ifAbsentPut: [ RwComponentLoadConfiguration newNamed: aComponentName for: self name ].
	^ component 
		addDefinedGroupName: self defaultGroupName includeGroups: #();
		comment: commentString;
		yourself