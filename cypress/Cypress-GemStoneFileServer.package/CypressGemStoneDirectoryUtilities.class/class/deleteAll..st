"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: utilities
"
deleteAll: aDirectory
  "Delete all the files and directories under the named directory.
	 Ensure we don't try to recursively delete . or .."

  self deleteAll: aDirectory rejecting: [ :filename | false ]