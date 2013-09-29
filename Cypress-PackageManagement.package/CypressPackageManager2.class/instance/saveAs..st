"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: Updating
"
saveAs: aKey
	"Save the receiver in the class' collection of named managers
	 under the specified key.
	 It will quietly replace anything already under that key."


	self savedPackageManagers at: aKey put: self.
