module.exports = {
  mode: "development",
  module: {
    rules: [
      {
        test: /\.(sass|scss|css)$/,
        use: ["style-loader", "css-loader", "sass-loader"],
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        use: [{ loader: "file-loader", options: { name: "[name].[ext]" } }],
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          { loader: "elm-hot-webpack-loader" },
          { loader: "elm-webpack-loader" },
        ],
      },
    ],
  },
};
