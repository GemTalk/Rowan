set up / teardown
cleanUpMethods

	methodsToCleanUp
		reverseDo: [:each | each methodClass removeSelector: each selector]