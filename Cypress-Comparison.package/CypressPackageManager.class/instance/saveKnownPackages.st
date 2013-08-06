"
notice: This work is protected by copyright. All rights reserved.
category: updating
"
saveKnownPackages

	self updateKnownPackages.
	((System myUserProfile resolveSymbol: #KnownCypressPackages)
		ifNil: 
			[(System myUserProfile objectNamed: #UserGlobals)
				addAssociation: #KnownCypressPackages -> Dictionary new])
			value: knownPackages