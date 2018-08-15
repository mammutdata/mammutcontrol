const path = require("path");
const webpack = require("webpack");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

const isWatch = process.argv.some(a => a === "--watch");
const isWebpackDevServer = process.argv.some(a => path.basename(a) === "webpack-dev-server");

const plugins = isWebpackDevServer || !isWatch ? [] : [
  function(){
    this.plugin("done", function(stats){
      process.stderr.write(stats.toString("errors-only"));
    });
  }
];

module.exports = {
  devServer: {
    contentBase: "public",
    port: 1337,
    stats: "errors-only"
  },

  entry: [
    "./src/loader.js",
    "./node_modules/materialize-css/dist/js/materialize.js",
    "./css/main.scss"
  ],

  mode: isWebpackDevServer ? "development" : "production",

  output: {
    path: path.resolve(__dirname, "public"),
    pathinfo: true,
    filename: "main.js"
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: "purs-loader",
            options: {
              bundle: true,
              src: [
                "bower_components/purescript-*/src/**/*.purs",
                "src/**/*.purs"
              ],
              psc: "psa",
              watch: isWebpackDevServer || isWatch,
              pscIde: false
            }
          }
        ]
      },

      {
        test: /\.scss$/,
        use: [
          MiniCssExtractPlugin.loader,
          { loader: "css-loader" },
          {
            loader: "sass-loader",
            options: {
              src: ["css/*/*.scss"]
            }
          }
        ]
      },

      {
        test: /\.(woff|woff2|eot|ttf|svg)$/,
        loader: "file-loader?name=fonts/[name].[ext]"
      }
    ]
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    }),

    new MiniCssExtractPlugin({
      // Options similar to the same options in webpackOptions.output
      // both options are optional
      filename: "[name].css",
      chunkFilename: "[id].css"
    }),

    function(){
      this.plugin('done', function(stats){
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ].concat(plugins)
};
