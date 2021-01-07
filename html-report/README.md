# parcel-preact-typescript

> Simple set-up using [Parcel](https://parceljs.org/), [Preact X](https://preactjs.com/) and [TypeScript](https://www.typescriptlang.org/).

This template provides the simplest possible set-up that combines Parcel, Preact X and TypeScript. Jest is used for testing.

## Getting started

Clone the repository and install its dependencies.

```bash
git clone git@github.com:metonym/parcel-preact-typescript.git
cd parcel-preact-typescript
yarn install
```

## Available scripts

### `yarn start`

Runs the app in development mode. Visit [http://localhost:1234](http://localhost:1234).

### `yarn build`

Builds the project for production. The build directory is `dist`; types will be written to `dist/index.d.ts`. Customize the build directory in the `outDir` configuration option in the [tsconfig.json](tsconfig.json).

### `yarn test`

Run the tests using the [jest](https://jestjs.io/) JavaScript testing framework.

The jest configuration for this project is in `package.json`:

```js
"jest": {
  "preset": "ts-jest"
}
```

## Customizing `tsconfig.json`

The default [`tsconfig.json`](tsconfig.json) contains the following:

```js
// tsconfig.json
{
  "compilerOptions": {
    "esModuleInterop": true,
    "jsx": "react",
    "jsxFactory": "h",
    "lib": ["dom", "esnext"],
    "module": "esnext",
    "moduleResolution": "node",
    "strict": true,
    "target": "es5"
  },
  "include": ["src"]
}
```

Note that `jsxFactory` must be `"h"` in order for preact to work with parcel.

### Decorators

Enable decorators ([stage 2](https://github.com/tc39/proposal-decorators)) by setting `compilerOptions.experimentalDecorators` to `true`:

```js
{
  "compilerOptions": {
    "experimentalDecorators": true
  }
}
```

## License

[MIT](LICENSE)
