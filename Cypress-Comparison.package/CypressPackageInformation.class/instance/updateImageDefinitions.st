"
notice: This work is protected by copyright. All rights reserved.
category: updating
"
updateImageDefinitions

	self
		imageDefinitions: (CypressPackageDefinition named: self name) snapshot
					definitions;
		imageCounts: self imageDefinitionCounts