"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testAuthorAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '-dhk'
			parsesToPackageName: ''
			author: 'dhk'
			branch: ''
			version: 0
