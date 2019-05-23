private
_issue481_loadProjectDefinition: projectName  packageName1: packageName1 packageName2: packageName2 symDictName: symDictName

	self
		_loadProjectDefinition: projectName
		packageNames: { packageName1 . packageName2}
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.