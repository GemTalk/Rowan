set up / teardown
cleanUpCategories

	categoriesToCleanUp reverseDo: [:each | each value removeCategory: each key]