#
# Authentication
#
echo -e ">>> Authentication !"

git remote set-url origin "git@github.com:quasiquoting.git"
git config --global user.email "quasiquoting@gmail.com"
git config --global user.name  "quasiquoting (via TravisCI)"

chmod 600 ~/.ssh/id_rsa

echo -e ">>> Copy config"
mv -fv .travis/ssh-config ~/.ssh/config
