"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testPackageBranchAuthorAndVersion
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements.v3-dhk.1'
			parsesToPackageName: 'Announcements'
			author: 'dhk'
			branch: 'v3'
			version: 1
