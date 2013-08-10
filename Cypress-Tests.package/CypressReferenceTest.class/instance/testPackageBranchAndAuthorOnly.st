testing
testPackageBranchAndAuthorOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements.v3-dhk'
			parsesToPackageName: 'Announcements'
			author: 'dhk'
			branch: 'v3'
			version: 0
