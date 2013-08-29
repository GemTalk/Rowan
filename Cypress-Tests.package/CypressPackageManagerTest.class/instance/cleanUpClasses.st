set up / teardown
cleanUpClasses

	classesToCleanUp reverseDo: [:each | each value removeKey: each key name]