# NStack: Composable, typed streams and microservices for data analytics

## Introduction

![Introduction](https://cdn.rawgit.com/nstack/nstack/c67fd1a9/images/readme-flowchart-intro.svg)

NStack is a compute platform that is ideal for data analytics because it makes integrating data, publishing code, and connecting it all together really simple.
<!-- Think of it like Bash-like, type-safe, piping for containerised microservices that live on your cloud. -->

1. You can turn disparate data-sources -- such as databases, 3rd-party APIs, or HTTP endpoints -- into streams of typed records. 

2. You can publish local code as functions on your cloud provider. Streams can be composed and connected with these functions using NStack's scripting language, and NStack automates all underlying infrastructure so you can focus on data-science instead of operations.

3. NStack provides end-to-end software life-cycle management for the data science process, including sharing and reuse, reproducibility, versioning, and runtime isolation.

## Getting Started

See the [website](https://nstack.com) for more information, or check out the [full documentation](https://docs.nstack.com).

### Intro Screencast

<a href="http://docs.nstack.com/en/latest/quick_start/index.html" target="_blank"><img src="https://asciinema.org/a/112733.png" width="600"/></a>


NStack is comprised of a CLI which runs on your machine, and a virtual machine which runs on the cloud.

### CLI Installation

The NStack CLI is available as self-contained executable for Linux, Windows, and macOS - binaries can be downloaded on our [releases page](https://github.com/nstack/nstack/releases).
Simply download `nstack-cli-{linux64,win64,macOS}` for your platform, uncompress, and run `nstack` from the Terminal/Command Prompt.

#### macOS

In addition to standalone download on the [releases page](https://github.com/nstack/nstack/releases), we have a [homebrew](https://brew.sh/) package that can easily be installed as follows,

```bash
$ brew tap nstack/nstack
$ brew cask install nstack-cli
```

#### Linux

We also provide RPM and DEB packages on the [releases page](https://github.com/nstack/nstack/releases) that will work with most common distros and can be installed via your system package manager.

<!--
We also have `yum` and `apt` repositories for Redhat and Debian- derived OSs that are updated on each release.

##### RedHat / Fedora / OpenSuse RPMs

A YUM/DNF repo for RedHat-based distros is located at http://distrepos.nstack.com/redhat - it includes both the `nstack-cli` and `nstack-server` packages,

```bash
sudo wget -O /etc/yum.repos.d/nstack.repo http://distrepos.nstack.com/redhat/nstack.repo
sudo dnf install nstack-cli
```

##### Ubuntu / Debian / Mint Debs

An Apt repo for Debian-based distros is located at http://distrepos.nstack.com/debian - it currently includes the `nstack-cli` package,

```bash
sudo wget -O /etc/sources.list.d/nstack.list http://distrepos.nstack.com/debian/nstack.list
sudo apt-get update
sudo apt-get install nstack-cli
```
-->

##### RPM Install

```bash
# change {version} as needed
dnf install https://github.com/nstack/nstack/releases/v{version}/nstack-cli-{version}.x86_64.rpm
```

##### DEB Install

```bash
# change {version} as needed
wget https://github.com/nstack/nstack/releases/v{version}/nstack-cli_{version}.amd64.deb
dpkg -i nstack-cli_{version}.amd64.deb
apt-get install -f
```

#### Compiling from source

NStack is built using [Haskell](https://wwww.haskell.org), to compile the CLI manually you will need the [Stack](https://www.haskellstack.org/) build tool. 
Once this is installed, run the following commands from the git project root,

```bash
# stack setup only needed on first compile
stack setup
stack build nstack-cli
# install to user's local executable directory
stack install nstack-cli
```

### Server Installation

#### PaaS

NStack offers a free-of-charge PaaS (Platform as a Service) for demo use, which means you can try NStack without installing the server. Note that the PaaS is intended as a sandbox and the environment is wiped daily at 0600 UTC.

You can register an account and immediately start using NStack using the following command:

``nstack register <username> <email>``

This will send you an email with your credentials and instructions on getting started.

#### Host your own NStack server

To install your own NStack server, we provide a self-contained appliance VM:
- an AMI for AWS EC2 (`ami-53a47245`)
- a `.raw` disk image for hosting on your virtual machine of choice

We also provide an RPM for installing directly on a Red Hat-like server. These are all available on the [releases page](https://github.com/nstack/nstack/releases).

<!--
We also provide an RPM and an associated Yum repository for installing directly on a Red Hat-like server

```bash
sudo wget -O /etc/yum.repos.d/nstack.repo http://distrepos.nstack.com/redhat/nstack.repo
sudo dnf install nstack-server
```
-->
## Examples

### Basic Example

![Example](https://cdn.rawgit.com/nstack/nstack/7ca03fc0/images/readme-flowchart-example.svg)

We can express this within the NStack scripting language locally as follows (just think of it as Bash for microservices).

```fsharp
module Demo:0.1.0 {
  import NStack.Transformers:0.1.4 as T
  import Acme.Classifiers:0.3.0 as C

  // our analytics workflow
  def workflow = Sources.Postgresql<(Text, Int)> 
                 | T.transform { strength = 5 }
                 | C.classify { model = "RandomForest" }
                 | Sinks.S3blob<Text>
}
```

We can then build, deploy, and start this workflow on an NStack Server from the NStack CLI on Linux, macOS, or Windows.

```bash
> nstack build
Building Demo:0.1.0
> nstack start Demo:0.1.0.workflow
Workflow started as process 3.
```
### More Examples

See https://github.com/nstack/nstack-examples for a range of examples, which you can clone and use immediately, including

* [demos](https://github.com/nstack/nstack-examples/tree/master/demos) - Basic examples that demonstrate creating methods and composing them together into workflows
* [nstack](https://github.com/nstack/nstack-examples/tree/master/nstack) - A selection of NStack utility modules, including methods for uploading to S3 and processing images
* [iris](https://github.com/nstack/nstack-examples/tree/master/iris) - A Python-based classifier using `scikit-leaarn` that showcases building more complex modules with system dependencies and in-built data-sets
* [movies](https://github.com/nstack/nstack-examples/tree/master/movies) - A complex worfklow composed from multiple individual services that processes movies data from the IMDB database -- demonstrating composition, filtering, service configuration, and partial workflow reuse

## What do people use NStack for?

### Productionising models
Productionise your models in the cloud without complex engineering, where they can be used in workflows and attached to data-sources. For instance, you can build a Random Forest classifier locally in Python, publish it to your cloud provider, and connect it to a streaming system, database, data-lake, or HTTP endpoint in under 10 minutes.

### Data Integration
Transform disparate and disconnected data-sources -- such as 3rd-party APIs, legacy infrastructure, or databases -- into streams of typed, structured records, which can be composed together. For instance, you could set up a workflow in the cloud which pipes the Twitter Ads API into your data lake (and even do some modelling in Python in-transit) in under 5 minutes.

## Features
- **Typed** Strongly-type your infrastructure and microservices to make them composable and secure
- **Streaming** Move your batch workloads and existing code to a streaming paradigm, without complex infrastructure
- **Fast** Really fast throughout by using the latest developments in the Linux kernel
- **Serverless** Modules are deployed as serverless, containerised, versioned, fully reproducible microservices 
- **Composable** Compose infrastructure in a statically typed workflow language to automate operations

## Concepts

#### Modules

A module is a piece of code that has been published to NStack -- for instance, a Python class. Modules are comprised of one or more **functions** -- in the same way a class of Python has one or more methods on it. Modules can have dependencies, like files or operating system packages -- for instance, your training data, or the ``scikit-learn`` package.

#### Functions

Functions are "serverless" functions which live on modules -- for instance, the `predict` method on your Python class. Functions on NStack are _typed_, which means you define what kind of data they can take as input, and the kind of data they output. For instance, you can say that your `predict` method only takes `Text` and returns `Integer`. This is important because it means they can be safely composed together and reused, with the NStack platform guaranteeing type safety.

#### Sources & Sinks

A source is something which emits a stream of data. A sink is something which can receive a stream of data. Examples  sources and sinks are databases, files, message-queues, and HTTP endpoints. Like modules, you can define the input and output types for your sources and sinks. 

#### Workflows

Modules, sources, and sinks can be combined -- or _composed_ -- together to build workflows. This is accomplished using the NStack Workflow Language, a simple bash-like scripting language for connecting streams and functions together.

#### Processes

When a workflow is started and is running in the cloud, it becomes a process.
