"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating - type
"
beConflictedWith: somePackageNames
	"Be designated as representing the prefix of one or more Known Package names."

	type := 'Conflicted Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Conflicts with the packages named ', self competingPackageNamesString