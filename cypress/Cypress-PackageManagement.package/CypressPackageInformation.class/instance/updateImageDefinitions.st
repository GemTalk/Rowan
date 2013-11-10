"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
updateImageDefinitions

	self
		imageDefinitions: (CypressPackageDefinition named: self name) snapshot
					definitions;
		imageCounts: self imageDefinitionCounts