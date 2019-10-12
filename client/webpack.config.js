const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
  entry: {
    app: [
      './src/index.js'
    ]
  },

  plugins: [
    new MiniCssExtractPlugin({}),
    new OptimizeCSSAssetsPlugin({}),
    // this copy plugin is the sadly the only option to
    // include our image assets into the distribution
    // via webpack
    // there is 'elm-assets-loader' but it doesn't work
    // with webpack >= 4.x
    new CopyWebpackPlugin([
      'src/img'
    ])
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
        loader:  'elm-webpack-loader',
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
