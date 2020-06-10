spago bundle-app --main Server.Main --to server-dist/server.js
cp package.json server-dist/package.json
cp package-lock.json server-dist/package-lock.json
cd server-dist
git add .
git commit -m "New build"
git push -f heroku server-builds:master
