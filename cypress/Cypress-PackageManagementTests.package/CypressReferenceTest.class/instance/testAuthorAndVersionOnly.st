"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testAuthorAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '-dhk.1'
			parsesToPackageName: ''
			author: 'dhk'
			branch: ''
			version: 1
