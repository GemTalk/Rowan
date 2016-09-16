"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Updating
"
removePackageManagerSavedAs: aKey
	"Remove the Package Manager previously saved under aKey, if there was one.
	 Answer it or nil if there was not one saved under that key."

	^self savedPackageManagers removeKey: aKey ifAbsent: []