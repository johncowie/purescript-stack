spago bundle-app --main Main --to frontend-dist/frontend.js
cp -r css frontend-dist/css
node_modules/browserify/bin/cmd.js frontend-dist/frontend.js -o frontend-dist/frontend.bundle.js
cd frontend-dist
rm frontend.js
git add .
git commit -m "New build"
git push -f origin gh-pages
