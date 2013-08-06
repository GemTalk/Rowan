"
notice: This work is protected by copyright. All rights reserved.
category: updating - type
"
beConflictedWith: somePackageNames
	"Be designated as representing the prefix of one or more Known Package names."

	type := 'Conflicted Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Conflicts with the packages named ', self competingPackageNamesString