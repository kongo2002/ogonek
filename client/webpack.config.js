const path = require('path');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

module.exports = {
  entry: {
    app: [
      './src/index.js'
    ]
  },

  plugins: [
    new MiniCssExtractPlugin()
  ],

  output: {
    path: path.resolve(__dirname + '/dist/static'),
    filename: '[name].js',
  },

  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          {
            loader: MiniCssExtractPlugin.loader,
          },
          "css-loader"
        ],
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file-loader',
        options: {
          name: '[name].[ext]',
          outputPath: '../',
        }
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack-loader?verbose=true&warn=true',
      },
    ],

    noParse: /\.elm$/,
  },

  devServer: {
    inline: true,
    stats: { colors: true },
  },
};

// vim: et sw=2 sts=2
