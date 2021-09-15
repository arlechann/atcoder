#!/bin/bash


cat << EOF > .gitignore
# バイナリ以外
/.vscode/
/Cargo.toml
/Cargo.lock
/target
/AtCoder-rs/target

EOF

echo "# バイナリファイル" >> .gitignore
find . -type f | grep -vE "(./.git|\.[^/]*$)" | sed 's/^\.//' >> .gitignore