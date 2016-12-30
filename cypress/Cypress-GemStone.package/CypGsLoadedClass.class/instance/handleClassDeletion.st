"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
handleClassDeletion
	"The class to which I refer has been deleted, so I must unregister myself."

	loadedPackage removeLoadedClass: self.
	ClassRegistry removeKey: handle classHistory