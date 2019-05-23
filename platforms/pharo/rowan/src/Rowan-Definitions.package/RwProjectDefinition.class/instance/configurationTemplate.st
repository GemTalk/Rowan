properties
configurationTemplate

	"Return a generic RwProjectLoadConfiguration based on the list of packages in the reciever"

	| config groupNames |
	config := RwProjectLoadConfiguration new
		name: 'Default';
		projectName: self specification specName;
		yourself.
	groupNames := self specification defaultGroupNames.
	groupNames
		do: [:groupName | config addDefinedGroupName: groupName ].
	config 
		conditionalPackagesAtConditions: { Rowan platform basePlatformAttribute } 
		andGroup: (groupNames at: 1) 
		addPackageNames: self packageNames.
	^ config
