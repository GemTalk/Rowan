"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeKnownPackages

	knownPackages := (System myUserProfile objectNamed: #KnownCypressPackages)
				ifNil: [Dictionary new]