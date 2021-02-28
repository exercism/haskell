#!/bin/sh
#
# Check that practice exercise descriptions are in sync with problem-specifications.

override_message="I have confirmed that no README check is needed"

# If *any* commit message contains the override message:
if git log origin/main..HEAD | grep -q "$override_message"; then
  echo "WARNING: You've overridden the README check, which applies to ALL commits in this PR."
  echo "No practice exercise description in this PR will be checked for changes."
  exit 0
fi

newline=$'\n  '

changed_practice_exercises() {
  branch_name="$(git rev-parse --abbrev-ref HEAD)"
  git diff --name-only "origin/main..$branch_name" | \
    grep -Po '(?<=^exercises/practice/)\w+' | sort -fu
}

missing_readmes=""
for exercise in $(changed_practice_exercises); do
  echo "Checking readme for $exercise"

  instructions_path="exercises/practice/$exercise/.docs/instructions.md"
  if [ ! -f "$instructions_path" ]; then
    missing_readmes="$missing_readmes$newline$exercise"
  fi
done

if [ -n "$missing_readmes" ]; then
  echo "Exercises missing instructions.md:$missing_readmes"
fi
if [ -n "$missing_readmes" ]; then
   exit 1
fi
