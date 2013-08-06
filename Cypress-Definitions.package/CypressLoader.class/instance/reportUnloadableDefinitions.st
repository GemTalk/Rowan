"
notice: This work is protected by copyright. All rights reserved.
category: loading
"
reportUnloadableDefinitions

	self unloadable isEmpty ifTrue: [^self].
	(CypressLoaderMissingClasses missingRequirementsMap: unloadable) signal.
