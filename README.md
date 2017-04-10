# NStack: Composable, typed streams and microservices for data analytics

## Introduction

![Introduction](https://cdn.rawgit.com/nstack/nstack/ebe779d9f560ba618b7804bbdceaa589801ea2ff/images/readme-flowchart-intro.svg)

NStack is a compute platform. It is ideal for data analytics because it makes integrating data, productionising code, and connecting it to that data really simple.
<!-- Think of it like Bash-like, type-safe, piping for containerised microservices that live on your cloud. -->
Firstly, it provides a way to turn disparate data-sources -- such as databases, 3rd-party APIs, or HTTP endpoints -- into streams of typed records. Secondly, it provides a way to publish local code as *functions* on your cloud provider. 
These streams can be composed with these functions using NStack's scripting language, and NStack automates all underlying infrastructure so you can focus on data-science instead of operations.


### Example

![Example](https://cdn.rawgit.com/nstack/nstack/ebe779d9f560ba618b7804bbdceaa589801ea2ff/images/readme-flowchart-example.svg)

We can express this within the NStack scripting language locally as follows, just think of it as Bash for containerised microservices,

```fsharp
module Demo:0.1.0 {
  import NStack.Transformers:0.1.4 as T
  import Acme.Classifiers:0.3.0 as C

  // our analytics workflow
  def workflow = Sources.Postgresql<(Int, Int, Text, CustomerRecord)> 
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


<!-- NStack is platform-agnostic, which means it can run anywhere. -->

See the [website](https://nstack.com) for more information, or check out the [full documentation](https://docs.nstack.com).


## Getting Started

NStack is comprised of a CLI which runs on your machine, and a virtual machine which runs on the cloud.

### CLI Installation

Our CLI is available for Linux, Mac, and Windows and the latest pre-compiled versions can be downloaded on our [releases page](https://github.com/nstack/nstack/releases). the CLI is distributed as a self-contained compressed executable for Linux, Windows, and macOS.

Download `nstack-cli-{linux64,win64,macOS}` for your platform, uncompress, and run `nstack` from the Terminal/Command Prompt

#### Linux

We also provide RPM and DEB packages on the [releases page](https://github.com/nstack/nstack/releases) that will work for most common distros. 

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

NStack is built using [Haskell](https://wwww.haskell.org), and to compile the CLI you will need the [Stack](https://www.haskellstack.org/
) build tool install. Once this is done, just run the following commands from the checked-out project source directory

```bash
# setup only needed on first compile
stack setup
stack build nstack-cli
# install to user's local executable directory
stack install nstack-cli
```

### Server Installation

#### PaaS

NStack offers a free-of-charge PaaS (Platform as a Service) for demo use, which means you can try NStack without installing the server. Note that the PaaS is intended as a sandbox and the environment is wiped daily at 0600 UTC.

To create a demo account, please go to https://nstack.com and fill out the demo web form. You'll shortly receive an email with instructions to point your install of the `nstack` tookit to our demo server.

#### Host your own server

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

## Examples

See https://github.com/nstack/nstack-examples

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
