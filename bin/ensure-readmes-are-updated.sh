#!/bin/sh

override_message="I have confirmed that no README check is needed"

# If *any* commit message contains the override message:
if git log origin/main..HEAD | grep -q "$override_message"; then
  echo "WARNING: You've overridden the README check, which applies to ALL commits in this PR."
  echo "No README in this PR will be checked for changes."
  exit 0
fi

configlet=$(which configlet)
if [ ! -x "$configlet" ]; then
    configlet=bin/configlet
fi
if [ ! -x "$configlet" ]; then
   echo "Improper configuration; configlet should exist in bin/ when this script is run"
   echo "Ping a Haskell track maintainer to fix this"
   exit 1
fi

if [ ! -d "problem-specifications" ]; then
   git clone https://github.com/exercism/problem-specifications.git problem-specifications
fi

newline=$'\n  '

missing_readmes=""
wrong_readmes=""
for exercise in $(git diff --name-only origin/main..$(git rev-parse --abbrev-ref HEAD) | grep exercises/ | cut -d'/' -f2 -s | sort -fu); do
   echo "Checking readme for $exercise"
   readme_path="exercises/${exercise}/README.md"
   if [ ! -f $readme_path ]; then
      missing_readmes="$missing_readmes$newline$exercise"
   else
      existing_readme_checksum=$(md5sum $readme_path | cut -d' ' -f1)
      # generate the new README
      $configlet generate . --only "$exercise" --spec-path "problem-specifications"
      generated_readme_checksum=$(md5sum $readme_path | cut -d' ' -f1)

      if [ $existing_readme_checksum != $generated_readme_checksum ]; then
         wrong_readmes="$wrong_readmes$newline$exercise"
      fi
   fi
done

if [ -n "$missing_readmes" ]; then
  echo "Exercises missing README.md:$missing_readmes"
fi
if [ -n "$wrong_readmes" ]; then
  echo "Exercises with out-of-date README.md:$wrong_readmes"
fi
if [ -n "$missing_readmes" -o -n "$wrong_readmes" ]; then
   exit 1
fi
