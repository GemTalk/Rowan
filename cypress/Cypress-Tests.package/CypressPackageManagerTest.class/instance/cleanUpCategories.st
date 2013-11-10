"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: set up / teardown
"
cleanUpCategories

	categoriesToCleanUp reverseDo: [:each | each value removeCategory: each key]