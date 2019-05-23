private
_readProjectSetForProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo

	self _readProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo.
	^ RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself
