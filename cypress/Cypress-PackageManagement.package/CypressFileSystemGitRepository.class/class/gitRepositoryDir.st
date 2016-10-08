"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
gitRepositoryDir
  ^ GitRepositoryDir ifNil: [ CypressFileUtilities current workingDirectory
        , CypressFileUtilities current pathNameDelimiter , 'cypress-git-repos' ]