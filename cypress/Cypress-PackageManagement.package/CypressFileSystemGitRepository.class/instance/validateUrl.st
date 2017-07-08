"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: validating - private
"
validateUrl
  "At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

  (self url projectPath isEmpty or: [ self url projectBranchOrTag isEmpty ])
    ifTrue: [ self error: self printString , ' should not be used with non-git URLs.' ]
