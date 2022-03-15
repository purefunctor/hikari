const path = require("path");

module.exports = {
  mode: "production",
  entry: "./public/index.js",
  output: {
    path: path.resolve(__dirname, "public"),
    filename: "hikari.js",
  },
  module: {
    rules: [
      {
        test: /\.js$/i,
        include: path.resolve(__dirname, "public"),
        use: {
          loader: "babel-loader",
          options: {
            presets: ["@babel/preset-env"],
          },
        },
      }
    ],
  },
  devServer: {
    static: {
      directory: path.resolve(__dirname, "public"),
    }
  },
};
