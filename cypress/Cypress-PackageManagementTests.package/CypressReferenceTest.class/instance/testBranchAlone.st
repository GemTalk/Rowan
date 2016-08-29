"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testBranchAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3'
			parsesToPackageName: ''
			author: ''
			branch: 'v3'
			version: 0
