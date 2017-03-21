# NStack: Type-safe, composable microservices for data analytics

NStack is a data analytics platform which makes productionising code and connecting it to data really simple. It provides a way to transform local code on your machine into *functions* which live on your cloud provider. These can be connected to data and event-sources using NStack's scripting language, and NStack automates all infrastructure and operations. 

NStack is platform-agnostic, which means it can can run anywhere.

```
screencast
```

## 1. Getting Started

NStack is comprised of a CLI which runs on your machine, and a server which runs on the cloud.

### CLI Installation
Mac: 
Windows:
Linux:

### Server Installation

NStack offers a free-of-charge PaaS (Platform as a Service), which means you can try NStack _without_ installing the server. Note that the PaaS is intended as a sandbox and is wiped ever 24 hours. 

To install your own NStack server, we provide an AWS AMI, a raw 

## 2. Why do people use NStack?

### Productionising models
Productionise your models in the cloud without complex engineering, where they can be used in workflows and attached to data-sources. For instance, you can build a Random Forest classifier locally in Python, publish it to your cloud provider, and connect it to a streaming system, database, or HTTP endpoint in under 10 minutes.

### Data Integration
NStack transforms disparate and disconnected data-sources -- such as 3rd-party APIs, legacy infrastructure, or databases into streams of typed, structured records, which can be composed together. For instance, you could set up a workflow in the cloud which pipes the Twitter Ads API into your data warehouse (and even do some modelling in Python in-transit) in under 5 minutes.

## 3. Features
- **Typed** Strongly-type your infrastructure and microservices to make them composable and secure
- **Streaming** Move your batch workloads and existing code to a streaming paradigm, without complex infrastructure
- **Fast** Really fast throughout by using the latest developments in the Linux kernel
- **Serverless** Modules are deployed as serverless, containerised, versioned, fully reproducible microservices 
- **Composable** Compose infrastructure in a statically typed workflow language to automate operations

## 4. Concepts

### Modules

A module is a piece of code that has been published to NStack -- for instance, a Python class. Modules are comprised of one or more **functions** -- in the same way a class of Python has one or more methods on it. Modules can have dependencies, like files or operating system packages -- for instance, training data, or the ``scikit-learn`` package.

### Functions

Functions are "serverless" functions which live on modules. For instance, the `predict` method on your Python class. Functions on NStack are `typed`, which means you can define what kind of data they can take as input, and the kind of data they output. This means they can be safely composed together.

### Sources & Sinks

A source is something which emits a stream of data. A sink is something which can receive a stream of data. Example sources and sinks are databases, files, message-queues, and HTTP endpoints. Like modules, you can define the input and output schemas for your sources and sinks. 

### Workflows

Modules, sources, and sinks can be combined to build workflows. This is accomplished using the NStack Workflow Language, a simple bash-like scripting language for connecting infrastructure and functions together.

### Processes

When a workflow is started and is running in the cloud, it becomes a process.

## 5. How NStack works

