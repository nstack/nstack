module NStack.Registry.Routes (module NStack.Registry.Routes) where

import Data.Aeson (FromJSON(..))
import qualified Data.ByteString                  as BS
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client (client, ClientM)
import qualified Data.Text    as T

import NStack.Registry.Types

type DockerRegistryV2API =
    --
    -- GET /v2/<name>/tags/list
    -- https://docs.docker.com/registry/spec/api/#listing-image-tags
    Capture "name" Name :> "tags" :> "list" :> Get '[JSON] TagContainer
    --
    -- GET /v2/_catalog
    -- https://docs.docker.com/registry/spec/api/#listing-repositories
    :<|> "catalog" :> Get '[JSON] CatalogContainer
    --
    -- GET /v2/<name>/manifests/<reference>
    -- https://docs.docker.com/registry/spec/api/#pulling-an-image-manifest
    :<|> Capture "name" Name :> "manifests" :> Capture "reference" Reference :> Header "Accept" AcceptHeader :> Get '[DockerImageManifestV2_2] DockerImageManifestV2_2
    --
    -- GET /v2/<name>/blobs/<digest>
    -- https://docs.docker.com/registry/spec/api/#pulling-a-layer
    :<|> Capture "name" Name :> "blobs" :> Capture "digest" Digest :> Get '[OctetStream] BS.ByteString
    --
    -- POST /v2/<name>/blobs/uploads/
    -- https://docs.docker.com/registry/spec/api/#pushing-a-layer
--     :<|> Capture "name" Name :> "blobs" :> "uploads" :> ReqBody '[OctetStream] BS.ByteString :> Post ErrorContainer
    --
    -- PUT /v2/<name>/manifests/<reference>
    -- https://docs.docker.com/registry/spec/api/#pushing-an-image-manifest
    :<|> Capture "name" Name :> "manifests" :> Capture "reference" Reference :> ReqBody '[DockerImageManifestV2_2] DockerImageManifestV2_2 :> Put '[JSON] ErrorContainer
    --
    -- POST /v2/<name>/blobs/uploads/?mount=<digest>&from=<src_name>
    -- https://docs.docker.com/registry/spec/api/#cross-repository-blob-mount
    -- TODO(jonboulle): this should handle ErrorContainer responses
    :<|> Capture "name" Name :> "blobs" :> "uploads" :> QueryParam "mount" Digest :> QueryParam "from" Name :> Post '[PlainText] T.Text


type Name = T.Text

type Reference = T.Text

type Tag = T.Text

-- TODO(jonboulle): stricter type here
type Digest = T.Text

type AcceptHeader = T.Text

data TagContainer = TagContainer 
    {
        name :: T.Text,
        tags :: [Tag]
    } deriving (Generic, Show);

instance FromJSON TagContainer

type Repository = T.Text

data CatalogContainer = CatalogContainer
    {
        repositories :: [Repository]
    } deriving (Generic, Show);

instance FromJSON CatalogContainer

data ErrorContainer = ErrorContainer
    {
        errors :: [Error]
    } deriving (Generic, Show);

instance FromJSON ErrorContainer

data Error = Error {
    code :: Int,
    message :: T.Text,
    detail :: T.Text
} deriving (Generic, Show);

instance FromJSON Error

registryAPI :: Proxy DockerRegistryV2API
registryAPI = Proxy

getTags :: Name -> ClientM TagContainer
getCatalog :: ClientM CatalogContainer
getImageManifest :: Name -> Reference -> Maybe AcceptHeader -> ClientM DockerImageManifestV2_2
getBlob :: Name -> Digest -> ClientM BS.ByteString
putManifest :: Name -> Reference -> DockerImageManifestV2_2 -> ClientM ErrorContainer
crossMount :: Name -> Maybe Digest -> Maybe Name -> ClientM T.Text
(getTags :<|> getCatalog :<|> getImageManifest :<|> getBlob :<|> putManifest :<|> crossMount) = client registryAPI