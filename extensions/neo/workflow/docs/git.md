# git hack something

[mav-178/another-test-pr] git add -A
[mav-178/another-test-pr] git stash -m "Git Town WIP"
Saved working directory and index state On mav-178/another-test-pr: Git Town WIP
[mav-178/another-test-pr] git checkout -b something main
Switched to a new branch 'something'
[something] git stash pop
On branch something
Changes to be committed:
  (use "git restore --staged <file>..." to unstage)
	new file:   FOO
	new file:   devex/editors/emacs/extensions/current/extensions.el
	new file:   git-town.toml
Dropped refs/stash@{0} (3bb584bbb33a42475b75e2fa27904b03b70d91a7)
[something] git restore --staged .
branch "something" is now a child of "main"
branch "something" is now a feature branch

# git append another-thing

[something] git add -A
[something] git stash -m "Git Town WIP"
Saved working directory and index state On something: Git Town WIP
[something] git checkout -b another-thing
Switched to a new branch 'another-thing'
[another-thing] git stash pop
On branch another-thing
Changes to be committed:
  (use "git restore --staged <file>..." to unstage)
	new file:   FOO
	new file:   git-town.toml
Dropped refs/stash@{0} (fff2641e66d2350457065b962e2d75cb14c084a5)
[another-thing] git restore --staged .

