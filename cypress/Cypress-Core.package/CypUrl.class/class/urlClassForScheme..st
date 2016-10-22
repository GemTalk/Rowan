"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
urlClassForScheme: scheme

	^ self allSubclasses
				detect: [ :urlClass | urlClass schemeName = scheme ]
				ifNone: [ CypCypressFileUrl ].