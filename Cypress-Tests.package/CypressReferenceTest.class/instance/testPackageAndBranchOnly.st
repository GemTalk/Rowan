"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testPackageAndBranchOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements.v3'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: 'v3'
			version: 0
