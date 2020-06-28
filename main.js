import Main from "./output/Main";

function main() {
  Main.dev()
}

// see https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function() {
    console.log("running main again");
    main();
  });
}

console.log("starting");
main();
