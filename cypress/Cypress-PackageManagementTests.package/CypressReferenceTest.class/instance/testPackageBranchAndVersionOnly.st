"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testPackageBranchAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements.v3.1'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: 'v3.1'
			version: 0;
		assert: 'Announcements.v3-.1'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: 'v3'
			version: 1
