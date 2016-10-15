"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testPackageAuthorAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements-dhk.1'
			parsesToPackageName: 'Announcements'
			author: 'dhk'
			branch: ''
			version: 1
