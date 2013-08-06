"
notice: This work is protected by copyright. All rights reserved.
category: initializing
"
initializeKnownPackages

	knownPackages := (System myUserProfile objectNamed: #KnownCypressPackages)
				ifNil: [Dictionary new]