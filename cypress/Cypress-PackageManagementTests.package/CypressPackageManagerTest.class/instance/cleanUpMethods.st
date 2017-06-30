"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: set up / teardown
"
cleanUpMethods

	methodsToCleanUp
		reverseDo: [:each | each inClass removeSelector: each selector]