"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
handleClassDeletion
	"The class to which I refer has been deleted, so I must unregister myself.
	Normally, I will have a loadedPackage, but if something interrupted my initialization it might be nil."

	loadedPackage ifNotNil: [loadedPackage removeLoadedClass: self].
	ClassRegistry removeKey: handle classHistory