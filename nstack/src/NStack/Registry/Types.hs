module NStack.Registry.Types (module NStack.Registry.Types) where

import Data.Aeson (FromJSON(..), ToJSON(..), encode)
import Data.Aeson.Types (fieldLabelModifier, genericToJSON, genericParseJSON, defaultOptions, Value(..), Parser, GFromJSON, GToJSON, Zero )
import Data.Map (Map)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Char as C
import qualified Data.Text as T
import GHC.Generics (Generic, Rep)

import Servant.API
import Servant.API.ContentTypes (eitherDecodeLenient)
import qualified Network.HTTP.Media as M


-- TODO(jonboulle): use Lens instead?
-- TODO(jonboulle): explicit encode/decode rather than derive from generic?

jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
    where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

fromDropPrefix :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
fromDropPrefix x = genericParseJSON (defaultOptions { fieldLabelModifier = map C.toLower . drop x }) . jsonLower

toDropPrefix :: (Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
toDropPrefix x = genericToJSON defaultOptions { fieldLabelModifier = map C.toLower . drop x }

-- |New Docker image manifest format (schemaVersion = 2)
-- application/vnd.docker.distribution.manifest.v2+json
-- https://docs.docker.com/registry/spec/manifest-v2-2/#image-manifest-field-descriptions
data DockerImageManifestV2_2 = DockerImageManifestV2_2 {
    imSchemaVersion :: Integer, -- must be 2
    imMediaType :: Text, -- must be application/vnd.docker.distribution.manifest.v2+json
    imConfig :: Descriptor, -- references a DockerImageConfigV1
    imLayers :: [Descriptor]
    } deriving (Generic, Show);

instance FromJSON DockerImageManifestV2_2 where
    parseJSON = fromDropPrefix 2

instance ToJSON DockerImageManifestV2_2 where
    toJSON = toDropPrefix 2


dockerImageManifestV2_2MediaType :: M.MediaType
dockerImageManifestV2_2MediaType = "application" M.// "vnd.docker.distribution.manifest.v2+json"

-- |Media type for use with Servant
-- https://hackage.haskell.org/package/servant-0.11/docs/Servant-API-ContentTypes.html
instance Accept DockerImageManifestV2_2 where
    contentType _ = dockerImageManifestV2_2MediaType

instance FromJSON a => MimeUnrender DockerImageManifestV2_2 a where
    mimeUnrender _ = eitherDecodeLenient

instance ToJSON a => MimeRender DockerImageManifestV2_2 a where
    mimeRender _ = encode

-- |Descriptor; no official Docker schema, just the OCI one
-- application/vnd.oci.descriptor.v1+json
-- https://github.com/opencontainers/image-spec/blob/master/descriptor.md
data Descriptor = Descriptor {
    dMediaType :: Text, -- REQUIRED
    dSize :: Integer, -- REQUIRED
    dDigest :: Text, -- REQUIRED
    dUrls :: Maybe [Text],  -- optional
    dPlatform :: Maybe Platform -- optional, ONLY when mediaType == application/vnd.docker.distribution.manifest.v2+json
--  dAnnotations :: Maybe [(Text, Text)]   -- optional, OCI extension
} deriving (Generic, Show);

instance FromJSON Descriptor where
    parseJSON = fromDropPrefix 1

instance ToJSON Descriptor where
    toJSON = toDropPrefix 1

data Platform = Platform {
    pArchitecture :: Text, -- REQUIRED
    pOs :: Text, -- REQUIRED
    pOs_version :: Maybe Text, -- optional
    pOs_features :: Maybe [Text], -- optional
    pVariant :: Maybe Text, -- optional
    fpFeatures :: Maybe [Text] -- optional
} deriving (Generic, Show);

instance FromJSON Platform where
    parseJSON = genericParseJSON opts . jsonLower where
        opts = defaultOptions { 
            -- os.version and os.features
            fieldLabelModifier = map (C.toLower . (\c -> if c=='_' then '.' else c)) . drop 1}

instance ToJSON Platform where
    toJSON = genericToJSON opts where
        opts = defaultOptions {
            -- os.version and os.features
            fieldLabelModifier = map (C.toLower . (\c -> if c=='_' then '.' else c)) . drop 1}

-- |Container config JSON
-- application/vnd.docker.container.image.v1+json 
-- 
-- In principle this is documented here:
-- https://github.com/moby/moby/blob/master/image/spec/v1.md#docker-image-specification-v100
-- but this is *incomplete*! (e.g. https://github.com/opencontainers/image-spec/pull/371)
--
-- Very similar (but not identical) to application/vnd.oci.image.config.v1+json
-- https://github.com/opencontainers/image-spec/blob/97ae57f204b5956aa313d453ead9094ff9056b32/config.md#properties
--
data DockerImageConfigV1 = DockerImageConfigV1 {
    icArchitecture :: String, -- REQUIRED
    icCreated :: Maybe String, -- optional, RFC3339 §5.6-formatted
    icAuthor :: Maybe String, -- optional
    icOs :: String, -- REQUIRED
    icConfig :: Maybe DockerExecutionConfig, -- optional
    icRootfs :: RootFSEntry, -- REQUIRED
    icHistory :: Maybe [HistoryEntry] -- optional
} deriving (Generic, Show);
-- Real example of this (from docker.io/library/busybox):
-- {
--     "architecture": "amd64",
--     "config": {
--          < see DockerExecutionConfig below >
--     },
--     "container": "f06386f489abf85537c32f826603214955d8731ccc5b45103d5548688abfa417",
--     "container_config": {
--       "Hostname": "dfb178f19369",
--       "Domainname": "",
--       "User": "",
--       "AttachStdin": false,
--       "AttachStdout": false,
--       "AttachStderr": false,
--       "Tty": false,
--       "OpenStdin": false,
--       "StdinOnce": false,
--       "Env": [
--         "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
--       ],
--       "Cmd": [
--         "/bin/sh",
--         "-c",
--         "#(nop) ",
--         "CMD [\"sh\"]"
--       ],
--       "ArgsEscaped": true,
--       "Image": "sha256:0348edacacf159ab6f19138182733dea8a418e06f526a2233d317c1f94549b4a",
--       "Volumes": null,
--       "WorkingDir": "",
--       "Entrypoint": null,
--       "OnBuild": null,
--       "Labels": {}
--     },
--     "created": "2017-08-23T22:31:12.496245278Z",
--     "docker_version": "17.03.2-ce",
--     "history": [
--       < see HistoryEntry below > 
--     ],
--     "os": "linux",
--     "rootfs": < see RootFSEntry below > 
--   }

instance FromJSON DockerImageConfigV1 where
    parseJSON = fromDropPrefix 2

instance ToJSON DockerImageConfigV1 where
    toJSON = toDropPrefix 2 

instance Accept DockerImageConfigV1 where
    contentType _ = 
        "application" M.// "vnd.docker.container.image.v1+json"

instance FromJSON a => MimeUnrender DockerImageConfigV1 a where
    mimeUnrender _ = eitherDecodeLenient

data RootFSEntry = RootFSEntry {
    rfType :: String, -- REQUIRED
    rfDiff_IDs :: [String] -- REQUIRED
} deriving (Generic, Show);
-- example:
--  {
--      "type": "layers",
--      "diff_ids": [
--          "sha256:6a749002dd6a65988a6696ca4d0c4cbe87145df74e3bf6feae4025ab28f420f2"
--      ]
--  }

instance FromJSON RootFSEntry where
    parseJSON = fromDropPrefix 2

instance ToJSON RootFSEntry where
    toJSON = toDropPrefix 2

data HistoryEntry = HistoryEntry {
    -- All fields optional
    hAuthor :: Maybe String,
    hCreated :: Maybe String,
    hCreated_By :: Maybe String,
    hEmpty_Layer :: Maybe Bool,
    hComment :: Maybe String
} deriving (Generic, Show);
-- example:
--  {
--      "created": "2017-08-23T22:31:12.496245278Z",
--      "created_by": "/bin/sh -c #(nop)  CMD [\"sh\"]",
--      "empty_layer": true
--  }

instance FromJSON HistoryEntry where
    parseJSON = fromDropPrefix 1

instance ToJSON HistoryEntry where
    toJSON = toDropPrefix 1

-- As above with the enclosing DockerImageConfig, this is not well documented.
-- In practise the best reference we have is probably the Docker code:
-- https://github.com/moby/moby/blob/b248de7e332b6e67b08a8981f68060e6ae629ccf/api/types/container/config.go#L30
-- but this is a superset of what's used in practise.
-- 
-- Similar implementation of the related OCI type:
-- https://github.com/opencontainers/image-spec/blob/97ae57f204b5956aa313d453ead9094ff9056b32/specs-go/v1/config.go#L23
--
data DockerExecutionConfig = DockerExecutionConfig {
    -- All fields optional
    user :: Maybe String,
    exposedPorts :: Maybe (Map String Value),
    env :: Maybe [String],
    entrypoint :: Maybe [String],
    cmd :: Maybe [String],
    volumes :: Maybe (Map String String),
    workingDir :: Maybe String,
    labels :: Maybe (Map String String),
    stopSignal :: Maybe String
} deriving (Generic, Show);
-- Real example of this (from docker.io/library/busybox):
-- {
--       "Hostname": "dfb178f19369",
--       "Domainname": "",
--       "User": "",
--       "AttachStdin": false,
--       "AttachStdout": false,
--       "AttachStderr": false,
--       "Tty": false,
--       "OpenStdin": false,
--       "StdinOnce": false,
--       "Env": [
--         "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
--       ],
--       "Cmd": [
--         "sh"
--       ],
--       "ArgsEscaped": true,
--       "Image": "sha256:0348edacacf159ab6f19138182733dea8a418e06f526a2233d317c1f94549b4a",
--       "Volumes": null,
--       "WorkingDir": "",
--       "Entrypoint": null,
--       "OnBuild": null,
--       "Labels": {}
-- }

instance ToJSON DockerExecutionConfig

instance FromJSON DockerExecutionConfig

-- |Manifest list, aka "fat manifest", pointing to one or more manifests
-- application/vnd.docker.distribution.manifest.list.v2+json 
-- https://docs.docker.com/registry/spec/manifest-v2-2/#manifest-list
--
-- TODO(jonboulle): implement.
-- While these are likely rare, per the documentation we should be able to handle
-- them being returned whenever we request an image manifest.
--
-- data DockerManifestListV2 = DockerManifestListV2 {
--    schemaVersion :: Int,
--    mediaType :: Text,
--    manifests :: [Descriptor]
--}

-- Should be unnecessary to implement:
-- application/vnd.docker.image.rootfs.diff.tar.gzip: "Layer", as a gzipped tar
-- application/vnd.docker.image.rootfs.foreign.diff.tar.gzip: "Layer", as a gzipped tar that should never be pushed
-- application/vnd.docker.distribution.manifest.v1+json: schema1 (old manifest format)
-- application/vnd.docker.plugin.v1+json: Plugin config JSON