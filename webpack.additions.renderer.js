var path = require("path");
var HtmlWebpackPlugin = require('html-webpack-plugin');

function resolve(filePath) {
    return path.join(__dirname, filePath)
}



module.exports = {
    entry: [resolve("src/Renderer/Renderer.fs.js"), resolve("src/Renderer/scss/main.scss")],
    resolve: {
        modules: ['node_modules']
    },
    output: {
        filename: "renderer.js"
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: path.resolve(__dirname, 'static/index.html')
        })
    ],
    module: {
        rules: [
            {
                test: /\.js$/,
                enforce: "pre",
                use: ["source-map-loader"],
            }

        ]
    }
}
