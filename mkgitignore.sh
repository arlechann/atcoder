#!/bin/bash


cat << EOF > .gitignore
# VScodeの設定
/.vscode/
/Cargo.toml
/Cargo.lock

EOF

echo "# バイナリファイル" >> .gitignore
find . -type f | grep -vE "(./.git|\.[^/]*$)" | sed 's/^\.//' >> .gitignore