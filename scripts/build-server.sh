spago bundle-app --main Server.Main --to server-dist/server.js
node_modules/noderify/bin.js server-dist/server.js -o server-dist/server.bundle.js
cd server-dist
git add .
git commit -m "New build"
git push -f heroku server-builds:master
