# NStack: Composable, typed streams and microservices for data analytics

## Introduction

NStack is a compute platform which is ideal for data analytics because it makes integrating data, productionising code, and connecting it to that data really simple. Firstly, it provides a way to turn disparate data-sources -- such as databases, 3rd-party APIs, or HTTP endpoints -- into streams of typed records. Secondly, it provides a way to publish local code as *functions* on your cloud provider. These streams can be composed with these functions using NStack's scripting language, and NStack automates all underlying infrastructure so you can focus on data-science instead of operations.

NStack is platform-agnostic, which means it can run anywhere.

See the [website](https://nstack.com) for more information, or check out the [full documentation](https://docs.nstack.com).

```
screencast
```

## Getting Started

NStack is comprised of a CLI which runs on your machine, and a virtual machine which runs on the cloud.

### CLI Installation

- Mac: 
- Windows:
- Linux:

### Server Installation

#### PaaS

NStack offers a free-of-charge PaaS (Platform as a Service) for demo use, which means you can try NStack without installing the server. Note that the PaaS is intended as a sandbox and the environment is wiped every 24 hours. 

To create a demo account, run:
```bash
$ nstack register
```

#### Host your own server

To install your own NStack server, we provide:
- an AMI for AWS
- a `.raw` disk image for hosting on your virtual machine of choice
- an RPM to install on a Red Hat-like server

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

## How NStack works

## Acknowledgments

## Contributing
## License
