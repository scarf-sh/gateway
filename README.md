# Scarf Gateway

Scarf Gateway is a single access-point to any digital artifact, no matter where they are hosted. When an software is served through Scarf Gateway, the traffic is seamlessly redirected to the right place, and you get details about how your software is being adopted and used.

Scarf Gateway supports Docker/OCI Containers, NPM packages, Python packages and any kind of executables.

## Building the project

Make sure you have configured:
  - Haskell GHC 9.4.4
  - Cabal 3.8.1.0

Run:

```
cabal install
```

Alternatively, this project uses [Nix Flakes](https://nixos.wiki/wiki/Flakes). The Gateway is set as the default package. So to build:

```
nix build
```


## Getting Started

CONFIGURE manifest here!

## Code of conduct

This project is for everyone. We ask that our users and contributors take a few minutes to review our Code of Conduct.

## License

Licensed under the Apache License, Version 2.0 (the "License"); you may not use these files except in compliance with the License. You may obtain a copy of the License at

```
http://www.apache.org/licenses/LICENSE-2.0
```

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.