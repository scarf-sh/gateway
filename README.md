<h1 align="center">
  <img src="/assets/gateway.svg" width="300" />
  <img referrerpolicy="no-referrer-when-downgrade" src="https://static.scarf.sh/a.png?x-pxid=55057c42-7e5c-4f06-b3c5-8745e7e0a06f" />
</h1>

Scarf Gateway is a universal redirect layer for any digital artifact or URL, anywhere online. Scarf Gateway acts much like a customizable link shortener that also lets you serve software like Docker containers, Python packages, or anything other kind of software you distribute. Host your Docker containers, NPM packages, PyPI packages, your binaries, and more, all from the same place. Scarf Gateway also makes it easy to switch between different registries without disrupting your public endpoints, allowing for a more flexible package distribution system that is decoupled from where your artifacts are hosted.

When requests are made to Scarf Gateway, the traffic is automatically redirected to the configured location, while ensuring seamless compatibility with popular package managers, container runtimes, and HTTP clients.

```bash
# Serve container pulls from Docker Hub, GHCR, etc via your domain
docker pull yourdomain.com/your-namespace/your-image
# Redirect Python package downloads PyPI, etc via your domain
pip install --extra-index-url yourdomain.com your-python-package
# Redirect HTTP requests, file downloads, etc to anywhere through your domain
wget yourdomain.com/linux/v1.2.0/your-tarball.tgz
```

Scarf Gateway is a core service that [Scarf](https://scarf.sh) runs internally. Scarf hosts a robust, globally distributed Gateway service, and augments it with extra functionality including:
  - A web dashboard for managing your package configuration
  - Robust analytics, data visualization, export, etc.

Scarf Gateway natively supports Docker/OCI Containers, Python packages, NPM packages, and any other file, URL, or URL template.

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

To use Scarf Gateway, you should first create a manifest with a set of redirection rules. For example:

my-manifest.json
```
{
  "rules": [
    {
      "repository-name": "library/hello-world",
      "package-id": "aaf2ec15-5244-484b-845a-ffd559e5f802",
      "domain": "testorg.docker.scarf.sh",
      "registry": "registry-1.docker.io",
      "type": "docker-v1"
     }
  ]
}
```

Then, just run the gateway using the manifest above:

```
scarf-gateway --manifest ./my-manifest.json
```

Now gateway is running at port 8081. You can pull docker images with `docker pull localhost:8081/hello-world`

## Gateway Rules

There are different manifest rules for docker, file, and python packages. The rule is composed by some basic properties:
- `type` - It can be `docker-v1` for docker images, `python-v1` for python packages, and `file-v1` for any kind of files or executable.
- `package-id` - A unique identifier for the package. It can be any string from your system.
- `domain` - A custom domain through where the package is served. Domains can be different for each file. E.g. `my-domain.com`

### Docker Rules

For Docker Packages (`docker-v1`) some additional properties are required:

- `repository-name` - The image name used by the docker pull command. E.g. `library/hello-world`, `library/nginx`
- `backend-registry` - The backend registry domain. The most common are `registry-1.docker.io` for Docker Hub, and `ghcr.io` for GitHub.

Example of Docker rule:

```
{
  "type": "docker-v1",
  "package-id": "6d3cda25-4ffc-461f-a1b8-72abf9ab3fc1",
  "domain": "registry.helloworlders",
  "repository-name": "library/hello-world", 
  "registry": "registry-1.docker.io"
}
```

### File Rules

For File Packages (`file-v1`) some additional properties are required:

- `incoming-path` - The URL path to download the file. E.g. `/minikube-{platform}-{version}.tar.gz`
- `outgoing-url` - The URL to redirect the file. E.g. `https://github.com/kubernetes/minikube/releases/downloads/minikube-{platform}-{version}.tar.gz`

Example of File rule:

```
{
  "type": "file-v1",
  "package-id": "a45901e6-8cad-4a27-8776-2349212c6a0d",
  "domain": "avi.gateway.scarf.sh",
  "incoming-path": "/{identifer}@{version}",
  "outgoing-url": "https://deno.land/x/{identifer}@{version}"
}
```
    
### Python Rules

For Python Packages (`python-v1`) some additional properties are required:

- `file-name` - The full name of the file. See examples below.
- `package-name` - The package name.
- `version` - The version of the package. 
- `hash-value` - The hash value. E.g (md5, 123...). 
- `gpg-sig` - A value of either true or false to indicate whether or not there is a GPG signature.
- `requires-python` - The Python version(s) that the distribution is guaranteed to be compatible with. The value follows the PEP 345 spec.
- `backend-url` - The backend URL to download the package.
- `backend-simple-index`- The manifest backend index. If not present, assume https://pypi.org/simple/

Example of Python rule

```
{
  "type": "python-v1",
  "package-id": "013e1ebd-3bc0-411d-b971-6ff1c20267c3",
  "domain": "fabioluz.gateway.scarf53.sh",
  "file-name": "numpy-1.6.1-cp27-none-macosx_10_6_intel.macosx_10_9_intel.macosx_10_9_x86_64.whl",
  "package-name": "numpy",
  "version": "1.6.1-cp27-none-macosx_10_6_intel.macosx_10_9_intel.macosx_10_9_x86_64.whl",
  "hash-value": {
      "hash": "5bd0a2a68903f1b286dd646f42f92f7de7bde6bbf3c4829a3f078400f48fa1e7",
      "type": "sha256"
  },
  "backend-url": "https://files.pythonhosted.org/packages/a6/b9/a9e4411c08a568a9558e4d4efc15cd26cf9f2f84e4d7ea800742fedb858c/numpy-1.6.1-cp27-none-macosx_10_6_intel.macosx_10_9_intel.macosx_10_9_x86_64.whl#sha256=5bd0a2a68903f1b286dd646f42f92f7de7bde6bbf3c4829a3f078400f48fa1e7"
  "backend-simple-index": "https://pypi.org/simple/"
}
```

For further details, please check [Scarf.Gateway.Manifest](/src/Scarf/Gateway/Manifest.hs)

## Caveats


### Bandwidth usage: redirect vs proxy

The gateway redirects whenever possible. Some versions of some container runtimes, however, do not properly authenticate with container registries when the client is redirected , and therefore the gateway is forced to proxy the requests to serve the download successfully. When Scarf Gateway must proxy, be prepared to pay for the bandwidth bill, as the container images themselves will pass through your infrastructure.

## Logging

Requests will be logged to stdout. Request details are in JSON format to be easily processed for any kind of analytics you might want to do. To learn more about getting advanced insights from Scarf Gateway traffic, check out the free and paid features of the entire [Scarf](https://scarf.sh) platform.

## Community

### Code of conduct

This project is for everyone. We ask that our users and contributors take a few minutes to review our [Code of Conduct](https://github.com/scarf-sh/code-of-conduct).

### Communication

* [Issues](https://github.com/scarf-sh/gateway/issues) can be used for open discussions. If you know someone who should hear about the message, tag them explicitly using the @username GitHub syntax.

* We also have a [slack server](https://join.slack.com/t/scarf-community/shared_invite/zt-1q9vpx13r-H9fy07psWSwM4SGF~vEsJA) that anyone can join where we can discuss about the gateway or all matters open source!
