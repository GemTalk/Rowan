"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testBranchAuthorAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3-dhk.1'
			parsesToPackageName: ''
			author: 'dhk'
			branch: 'v3'
			version: 1
