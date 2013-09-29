"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: set up / teardown
"
cleanUpMethods

	methodsToCleanUp
		reverseDo: [:each | each methodClass removeSelector: each selector]