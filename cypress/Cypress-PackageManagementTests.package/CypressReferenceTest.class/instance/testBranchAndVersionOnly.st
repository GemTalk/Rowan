"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testBranchAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3.1'
			parsesToPackageName: ''
			author: ''
			branch: 'v3.1'
			version: 0;
		assert: '.v3-.1'
			parsesToPackageName: ''
			author: ''
			branch: 'v3'
			version: 1
