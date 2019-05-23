visiting
visitProjectCompoundConfiguration: aProjectCompoundConfiguration

	(visitedConfigurationNames includes: aProjectCompoundConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aProjectCompoundConfiguration. 

	self _addPackageNames: aProjectCompoundConfiguration packageNames for: aProjectCompoundConfiguration.

	aProjectCompoundConfiguration _configurations
		do: [:config | config acceptCompoundVisitor: self ].
