#!/bin/bash

# Check if the hooks directory exists
if [ ! -d .git/hooks ]; then
    echo "Error: .git/hooks directory not found. Are you in the root of the repository?"
    exit 1
fi

chmod +x hooks/pre-commit

# Copy or symlink the pre-commit hook
echo "Installing pre-commit hook..."
cp hooks/pre-commit .git/hooks/pre-commit || \
    ln -s ../../hooks/pre-commit .git/hooks/pre-commit

echo "Pre-commit hook installed successfully."

