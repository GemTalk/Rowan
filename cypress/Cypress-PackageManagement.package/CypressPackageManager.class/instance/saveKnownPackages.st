"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating - private
"
saveKnownPackages

	self updateKnownPackages.
	((System myUserProfile resolveSymbol: #KnownCypressPackages)
		ifNil: 
			[(System myUserProfile objectNamed: #UserGlobals)
				addAssociation: #KnownCypressPackages -> Dictionary new])
			value: knownPackages